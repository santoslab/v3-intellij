/*
 Copyright (c) 2016, Robby, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum.intellij.logika.action

import java.awt.event.MouseEvent
import java.awt.{Color, Font}
import java.util.concurrent._
import javax.swing.Icon

import com.intellij.openapi.editor.event._

import com.intellij.notification.{NotificationType, Notification, Notifications}
import com.intellij.openapi.actionSystem._
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.markup._
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.util._
import com.intellij.openapi.wm.StatusBarWidget.{IconPresentation, WidgetPresentation, PlatformType}
import com.intellij.openapi.wm.{StatusBar, StatusBarWidget, WindowManager}
import com.intellij.ui.JBColor
import com.intellij.util.Consumer
import org.sireum.intellij.SireumApplicationComponent
import org.sireum.intellij.logika.LogikaConfigurable
import org.sireum.intellij.logika.lexer.Lexer
import org.sireum.logika.message._
import org.sireum.util._
import LogikaAction._

object LogikaCheckAction {

  object EditorEnabled

  val icons = {
    var r = (0 to 6).map(n => IconLoader.getIcon(s"/logika/icon/logika-$n.png"))
    r = r.dropRight(1)
    r ++= r.reverse
    r = r.dropRight(1)
    r
  }
  val icon = icons(0)
  val gutterErrorIcon = IconLoader.getIcon("/logika/icon/logika-gutter-error.png")
  val gutterWarningIcon = IconLoader.getIcon("/logika/icon/logika-gutter-warning.png")
  val gutterInfoIcon = IconLoader.getIcon("/logika/icon/logika-gutter-info.png")
  val verifiedInfoIcon = IconLoader.getIcon("/logika/icon/logika-verified-info.png")
  val queue = new LinkedBlockingQueue[String]()
  val editorMap = mmapEmpty[String, (Project, Editor)]
  val logikaKey = new Key[EditorEnabled.type]("Logika")
  val analysisDataKey = new Key[ISeq[RangeHighlighter]]("Logika Analysis Data")
  var request: Option[Request] = None
  var processInit = false
  var terminated = false

  final case class Request(time: Long, requestId: String,
                           project: Project, editor: Editor,
                           msgGen: () => String)

  sealed trait LogikaTextAttributes

  object ErrorTextAttributes
    extends TextAttributes(null, null, JBColor.red, EffectType.WAVE_UNDERSCORE, Font.PLAIN)
    with LogikaTextAttributes {
    setErrorStripeColor(Color.red)
  }

  object WarningTextAttributes
    extends TextAttributes(null, null, JBColor.orange, EffectType.WAVE_UNDERSCORE, Font.PLAIN)
    with LogikaTextAttributes {
    setErrorStripeColor(Color.yellow)
  }

  object InfoTextAttributes
    extends TextAttributes(null, null, JBColor.green, EffectType.WAVE_UNDERSCORE, Font.PLAIN)
    with LogikaTextAttributes {
    setErrorStripeColor(Color.green)
  }

  def init(p: Project): Unit = {
    if (!processInit) {
      processInit =
        SireumApplicationComponent.getSireumProcess(p,
          queue, { s =>
            if (s.trim != "")
              Message.unpickleOutput[OutputMessage](s) match {
                case r: Result => processResult(r)
              }
          }, "logika", "--server")

      val statusBar = WindowManager.getInstance().getStatusBar(p)
      var frame = 0
      val statusIdle = "Sireum Logika is idle"
      val statusWorking = "Sireum Logika is working"
      var statusTooltip = statusIdle
      val statusBarWidget = new StatusBarWidget {
        override def ID(): String = "Sireum Logika"

        override def install(statusBar: StatusBar): Unit = {}

        override def getPresentation(`type`: PlatformType): WidgetPresentation =
          new IconPresentation {
            override def getClickConsumer: Consumer[MouseEvent] = null

            override def getTooltipText: String = statusTooltip

            override def getIcon: Icon = icons(frame)
          }

        override def dispose(): Unit = {}
      }
      statusBar.addWidget(statusBarWidget)
      statusBar.updateWidget(statusBarWidget.ID())
      val t = new Thread {
        override def run(): Unit = {
          val defaultFrame = icons.length / 2 + 1
          val countLimit = 2
          var count = 0
          while (!terminated) {
            if (editorMap.nonEmpty) {
              frame = (frame + 1) % icons.length
              statusTooltip = statusWorking
              statusBar.updateWidget(statusBarWidget.ID())
            } else {
              val f = frame
              frame = defaultFrame
              statusTooltip = statusIdle
              if (f != defaultFrame)
                statusBar.updateWidget(statusBarWidget.ID())
            }
            this.synchronized {
              request match {
                case Some(r: Request) =>
                  if (System.currentTimeMillis - r.time > LogikaConfigurable.idle) {
                    request = None
                    editorMap.synchronized {
                      editorMap(r.requestId) = (r.project, r.editor)
                    }
                    queue.add(r.msgGen())
                  }
                case None =>
              }
            }
            Thread.sleep(175)
          }
          statusBar.removeWidget(statusBarWidget.ID())
        }
      }
      t.setDaemon(true)
      t.start()
    }
  }

  def isEnabled(editor: Editor): Boolean =
    EditorEnabled == editor.getUserData(logikaKey)

  def analyze(project: Project, editor: Editor, isSilent: Boolean): Unit = {
    if (!isEnabled(editor)) return
    ApplicationManager.getApplication.invokeLater(
      (() => Lexer.addSyntaxHighlighter(editor)): Runnable,
      ((_: Any) => editor.isDisposed): Condition[Any])
    init(project)
    val input = editor.getDocument.getText
    val isProgramming =
      getFileExt(project) match {
        case ".scala" | ".sc" => true
        case _ => false
      }
    val proofs = ivector(ProofFile(Some(getFilePath(project).get), input))
    val (t, requestId) = {
      val t = System.currentTimeMillis
      val id = t.toString
      (t, id)
    }
    def f(): String = {
      Message.pickleInput(Check(requestId, isSilent,
        isProgramming, proofs, lastOnly = false,
        autoEnabled = LogikaConfigurable.autoEnabled,
        timeout = LogikaConfigurable.timeout,
        checkSat = LogikaConfigurable.checkSat))
    }
    if (isSilent) {
      this.synchronized {
        request = Some(Request(t, requestId, project, editor, f))
      }
    } else {
      editorMap.synchronized {
        this.synchronized {
          request match {
            case Some(r: Request) =>
              editorMap -= r.requestId
            case _ =>
          }
          request = None
        }
        editorMap(requestId) = (project, editor)
      }
      queue.add(f())
    }
  }

  def enableEditor(editor: Editor): Unit = {
    editor.putUserData(logikaKey, EditorEnabled)
  }

  def editorOpened(project: Project, editor: Editor): Unit = {
    val ext = getFileExt(project)
    if (!fileExts.contains(ext)) return
    if (autoFileExts.contains(ext)) {
      enableEditor(editor)
      analyze(project, editor, isSilent = true)
    }
    editor.getDocument.addDocumentListener(new DocumentListener {
      override def documentChanged(event: DocumentEvent): Unit = {
        analyze(project, editor, isSilent = true)
      }

      override def beforeDocumentChange(event: DocumentEvent): Unit = {}
    })
    editor.addEditorMouseMotionListener(new EditorMouseMotionListener {
      override def mouseMoved(e: EditorMouseEvent): Unit = {
        if (!EditorMouseEventArea.EDITING_AREA.equals(e.getArea))
          return
        val rhs = editor.getUserData(analysisDataKey)
        if (rhs == null) return
        val component = editor.getContentComponent
        val point = e.getMouseEvent.getPoint
        val pos = editor.xyToLogicalPosition(point)
        val offset = editor.logicalPositionToOffset(pos)
        for (rh <- rhs)
          if (rh.getStartOffset <= offset && offset <= rh.getEndOffset) {
            component.setToolTipText(rh.getErrorStripeTooltip.toString)
            return
          }
        component.setToolTipText(null)
      }

      override def mouseDragged(e: EditorMouseEvent): Unit = {}
    })
  }

  def notifyHelper(project: Project, tags: ISeq[Tag]): Unit = {
    def notify(n: Notification): Unit =
      new Thread() {
        override def run(): Unit = {
          Notifications.Bus.notify(n, project)
          Thread.sleep(5000)
          ApplicationManager.getApplication.invokeLater(() => n.expire())
        }
      }.start()

    val lineSep = scala.util.Properties.lineSeparator
    val enlTags = tags.filter(_.isInstanceOf[ErrorTag])
    if (enlTags.nonEmpty) {
      notify(new Notification(
        "Sireum Logika", "Logika Error",
        enlTags.map(_.asInstanceOf[MessageTag].message).mkString(lineSep),
        NotificationType.ERROR, null))
    }
    val wnlTags = tags.filter(_.isInstanceOf[WarningTag])
    if (wnlTags.nonEmpty) {
      notify(new Notification(
        "Sireum Logika", "Logika Warning",
        wnlTags.map(_.asInstanceOf[MessageTag].message).mkString(lineSep),
        NotificationType.WARNING, null))
    }
    val inlTags = tags.filter(_.isInstanceOf[InfoTag])
    if (inlTags.nonEmpty) {
      val msg = inlTags.map(_.asInstanceOf[MessageTag].message).mkString(lineSep)
      val isVerified = msg.contains("is accepted")
      val (title, icon) =
        if (isVerified) ("Logika Verified", verifiedInfoIcon)
        else ("Logika Information", null)
      notify(new Notification("Sireum Logika", title, msg,
        NotificationType.INFORMATION, null) {
        override def getIcon: Icon = icon
      })
    }
  }

  def processResult(r: Result): Unit =
    ApplicationManager.getApplication.invokeLater(() => analysisDataKey.synchronized {
      val tags = r.tags
      val (project, editor) = editorMap.synchronized {
        val pe = editorMap(r.requestId)
        editorMap -= r.requestId
        pe
      }
      val mm = editor.getMarkupModel
      val rhs = editor.getUserData(analysisDataKey)
      if (rhs != null)
        for (rh <- rhs)
          mm.removeHighlighter(rh)
      editor.putUserData(analysisDataKey, null)
      val (lTags, nlTags) = tags.partition(_.isInstanceOf[UriTag with LocationInfoTag with MessageTag])
      if (!r.isSilent) notifyHelper(project, nlTags)
      if (lTags.isEmpty) return
      if (!editor.isDisposed) {
        val mm = editor.getMarkupModel
        var rhs = ivectorEmpty[RangeHighlighter]
        for (lTag <- lTags) (lTag: @unchecked) match {
          case tag: UriTag with LocationInfoTag with MessageTag =>
            val start = tag.offset
            val end = tag.offset + tag.length
            val (ta, icon) = tag match {
              case _: ErrorTag | _: InternalError => (ErrorTextAttributes, gutterErrorIcon)
              case _: WarningTag => (WarningTextAttributes, gutterWarningIcon)
              case _: InfoTag => (InfoTextAttributes, gutterInfoIcon)
            }
            val rh = mm.addRangeHighlighter(start, end, 1000000, ta, HighlighterTargetArea.EXACT_RANGE)
            rh.setErrorStripeTooltip(tag.message)
            rh.setThinErrorStripeMark(false)
            rh.setGutterIconRenderer(new GutterIconRenderer {
              override def getIcon = icon

              override def getTooltipText = tag.message

              override def equals(other: Any) = false

              override def hashCode = System.identityHashCode(this)
            })
            rhs :+= rh
        }
        editor.putUserData(analysisDataKey, rhs)
      }
    })
}

import LogikaCheckAction._

private class LogikaCheckAction extends LogikaAction {

  // init
  {
    getTemplatePresentation.setIcon(icon)

    val am = ActionManager.getInstance

    val runGroup = am.getAction("SireumLogikaGroup").
      asInstanceOf[DefaultActionGroup]
    runGroup.addAction(this, Constraints.FIRST)
  }

  override def actionPerformed(e: AnActionEvent): Unit = {
    e.getPresentation.setEnabled(false)
    val project = e.getProject
    val editor = FileEditorManager.
      getInstance(project).getSelectedTextEditor
    if (editor == null) return
    enableEditor(editor)
    analyze(project, editor, isSilent = false)
    e.getPresentation.setEnabled(true)
  }
}