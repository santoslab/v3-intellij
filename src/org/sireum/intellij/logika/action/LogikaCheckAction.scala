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

import java.awt.Font
import java.awt.event.MouseEvent
import java.util.concurrent._
import javax.swing.Icon

import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.editor.event._

import com.intellij.notification.{NotificationType, Notification, Notifications}
import com.intellij.openapi.actionSystem._
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.markup._
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.Messages
import com.intellij.openapi.util._
import com.intellij.openapi.wm.StatusBarWidget.{IconPresentation, WidgetPresentation, PlatformType}
import com.intellij.openapi.wm.impl.ToolWindowImpl
import com.intellij.openapi.wm.{StatusBar, StatusBarWidget, WindowManager}
import com.intellij.util.Consumer
import org.sireum.intellij.{Util, SireumToolWindowFactory, SireumApplicationComponent}
import org.sireum.intellij.logika.{LogikaFileType, LogikaConfigurable}
import org.sireum.intellij.logika.lexer.Lexer
import org.sireum.logika.message._
import org.sireum.util._

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
  val gutterHintIcon = IconLoader.getIcon("/logika/icon/logika-gutter-hint.png")
  val verifiedInfoIcon = IconLoader.getIcon("/logika/icon/logika-verified-info.png")
  val queue = new LinkedBlockingQueue[String]()
  val editorMap = mmapEmpty[String, (Project, Editor)]
  val logikaKey = new Key[EditorEnabled.type]("Logika")
  val analysisDataKey = new Key[ISeq[RangeHighlighter]]("Logika Analysis Data")
  val statusKey = new Key[Boolean]("Logika Analysis Status")
  var request: Option[Request] = None
  var processInit: Option[scala.sys.process.Process] = None
  var terminated = false

  final case class Request(time: Long, requestId: String,
                           project: Project, editor: Editor,
                           msgGen: () => String)

  def init(p: Project): Unit = {
    if (processInit.isEmpty) {
      processInit =
        SireumApplicationComponent.getSireumProcess(p,
          queue, { s =>
            if (s.trim != "")
              Message.unpickleOutput[OutputMessage](s) match {
                case r: Result => processResult(r)
              }
          }, "logika", "--server")
      if (processInit.isEmpty) return
      val statusBar = WindowManager.getInstance().getStatusBar(p)
      var frame = 0
      val statusIdle = "Sireum Logika is idle"
      val statusWaiting = "Sireum Logika is waiting to work"
      val statusWorking = "Sireum Logika is working"
      var statusTooltip = statusIdle
      var shutdown = false
      lazy val statusBarWidget: StatusBarWidget = new StatusBarWidget {

        override def ID(): String = "Sireum Logika"

        override def install(statusBar: StatusBar): Unit = {}

        override def getPresentation(`type`: PlatformType): WidgetPresentation =
          new IconPresentation {
            override def getClickConsumer: Consumer[MouseEvent] = (e) =>
              if (Messages.showYesNoDialog(
                p, "Shutdown Sireum Logika background server?",
                "Sireum Logika", null) == Messages.YES)
                editorMap.synchronized {
                  this.synchronized {
                    request = None
                    editorMap.clear()
                    processInit.foreach(_.destroy())
                    processInit = None
                    shutdown = true
                    statusBar.removeWidget(statusBarWidget.ID())
                  }
                }

            override def getTooltipText: String =
              statusTooltip + " (click to shutdown)."

            override def getIcon: Icon = icons(frame)
          }

        override def dispose(): Unit = {}
      }
      statusBar.addWidget(statusBarWidget)
      statusBar.updateWidget(statusBarWidget.ID())
      val t = new Thread {
        override def run(): Unit = {
          val defaultFrame = icons.length / 2 + 1
          while (!terminated && !shutdown) {
            if (editorMap.nonEmpty || request.nonEmpty) {
              frame = (frame + 1) % icons.length
              statusTooltip =
                if (editorMap.nonEmpty) statusWorking
                else statusWaiting
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
    if (LogikaConfigurable.syntaxHighlighting)
      ApplicationManager.getApplication.invokeLater(
        (() => Lexer.addSyntaxHighlighter(project, editor)): Runnable,
        ((_: Any) => editor.isDisposed): Condition[Any])
    if (!LogikaConfigurable.backgroundAnalysis) return
    init(project)
    val input = editor.getDocument.getText
    val proofs = ivector(ProofFile(Some(Util.getFilePath(project).get), input))
    val (t, requestId) = {
      val t = System.currentTimeMillis
      val id = t.toString
      (t, id)
    }
    def f(): String = {
      Message.pickleInput(Check(requestId, isSilent,
        LogikaConfigurable.hint, proofs, lastOnly = false,
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

  def enableEditor(project: Project, editor: Editor): Unit = {
    if (editor.getUserData(logikaKey) != null) return
    editor.putUserData(logikaKey, EditorEnabled)
    editor.getDocument.addDocumentListener(new DocumentListener {
      override def documentChanged(event: DocumentEvent): Unit = {
        if (LogikaConfigurable.backgroundAnalysis)
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
        for (rh <- rhs if rh.getErrorStripeTooltip != null)
          if (rh.getStartOffset <= offset && offset <= rh.getEndOffset) {
            component.setToolTipText(rh.getErrorStripeTooltip.toString)
            return
          }
        component.setToolTipText(null)
      }

      override def mouseDragged(e: EditorMouseEvent): Unit = {}
    })
  }

  def editorOpened(project: Project, editor: Editor): Unit = {
    val ext = Util.getFileExt(project)
    if (!LogikaConfigurable.allFileExts.contains(ext)) return
    if (LogikaFileType.extensions.contains(ext)) {
      enableEditor(project, editor)
      editor.putUserData(statusKey, false)
      analyze(project, editor, isSilent = true)
    }
  }

  def notify(n: Notification, project: Project): Unit =
    new Thread() {
      override def run(): Unit = {
        Notifications.Bus.notify(n, project)
        Thread.sleep(5000)
        ApplicationManager.getApplication.invokeLater(() => n.expire())
      }
    }.start()

  def notifyHelper(projectOpt: Option[Project], editorOpt: Option[Editor],
                   isSilent: Boolean, tags: ISeq[Tag]): Unit = {

    val project = projectOpt.orNull
    val statusOpt = editorOpt.map(_.getUserData(statusKey))
    val lineSep = scala.util.Properties.lineSeparator
    val ienlTags = tags.filter(_.isInstanceOf[InternalErrorTag])
    if (ienlTags.nonEmpty) {
      notify(new Notification(
        "Sireum Logika", "Logika Internal Error",
        ienlTags.map(_.asInstanceOf[MessageTag].message).mkString(lineSep),
        NotificationType.ERROR), project)
      editorOpt.foreach(_.putUserData(statusKey, false))
    }
    val enlTags = tags.filter(_.isInstanceOf[ErrorTag])
    if (enlTags.nonEmpty) {
      if (!isSilent || statusOpt.getOrElse(true))
        notify(new Notification(
          "Sireum Logika", "Logika Error",
          enlTags.map(_.asInstanceOf[MessageTag].message).mkString(lineSep),
          NotificationType.ERROR), project)
      editorOpt.foreach(_.putUserData(statusKey, false))
    }
    val wnlTags = tags.filter(_.isInstanceOf[WarningTag])
    if (wnlTags.nonEmpty && !isSilent) {
      notify(new Notification(
        "Sireum Logika", "Logika Warning",
        wnlTags.map(_.asInstanceOf[MessageTag].message).mkString(lineSep),
        NotificationType.WARNING, null), project)
    }
    val inlTags = tags.filter(_.isInstanceOf[InfoTag])
    if (inlTags.nonEmpty) {
      val msg = inlTags.map(_.asInstanceOf[MessageTag].message).mkString(lineSep)
      val isVerified = msg.contains("is accepted")
      val (title, icon) =
        if (isVerified) {
          editorOpt.foreach(_.putUserData(statusKey, true))
          ("Logika Verified", verifiedInfoIcon)
        } else ("Logika Information", null)
      if (!isSilent || !(isVerified && statusOpt.getOrElse(false)))
        notify(new Notification("Sireum Logika", title, msg,
          NotificationType.INFORMATION, null) {
          override def getIcon: Icon = icon
        }, project)
    }
  }

  def processResult(r: Result): Unit =
    ApplicationManager.getApplication.invokeLater(() => analysisDataKey.synchronized {
      val tags = r.tags
      if (r.requestId == "") {
        editorMap.synchronized {
          editorMap.clear()
        }
        notifyHelper(None, None, isSilent = false, tags)
        return
      }
      val (project, editor) = editorMap.synchronized {
        editorMap.get(r.requestId) match {
          case Some(pe) =>
            editorMap -= r.requestId
            pe
          case _ =>
            notifyHelper(None, None, isSilent = false, tags)
            return
        }
      }
      if (!editor.isDisposed) {
        val mm = editor.getMarkupModel
        var rhs = editor.getUserData(analysisDataKey)
        if (rhs != null)
          for (rh <- rhs)
            mm.removeHighlighter(rh)
        editor.putUserData(analysisDataKey, null)
        val (lTags, nlTags) = tags.partition(
          _.isInstanceOf[UriTag with LocationInfoTag with MessageTag with KindTag with SeverityTag])
        notifyHelper(Some(project), Some(editor), r.isSilent, nlTags)
        if (lTags.isEmpty) return
        rhs = ivectorEmpty[RangeHighlighter]
        val cs = editor.getColorsScheme
        val errorColor = cs.getAttributes(
          TextAttributesKey.find("ERRORS_ATTRIBUTES")).getErrorStripeColor
        val (errorIcon, errorAttr) =
          if (LogikaConfigurable.underwave) {
            (gutterErrorIcon, new TextAttributes(null, null, errorColor, EffectType.WAVE_UNDERSCORE, Font.PLAIN))
          } else {
            (null, new TextAttributes(null, errorColor, null, null, Font.PLAIN))
          }
        errorAttr.setErrorStripeColor(errorColor)
        val warningColor = cs.getAttributes(TextAttributesKey.find("WARNING_ATTRIBUTES")).getErrorStripeColor
        val (warningIcon, warningAttr) =
          if (LogikaConfigurable.underwave) {
            (gutterWarningIcon, new TextAttributes(null, null, warningColor, EffectType.WAVE_UNDERSCORE, Font.PLAIN))
          } else {
            (null, new TextAttributes(null, warningColor, null, null, Font.PLAIN))
          }
        val infoColor = cs.getAttributes(TextAttributesKey.find("TYPO")).getEffectColor
        val (infoIcon, infoAttr) =
          if (LogikaConfigurable.underwave) {
            (gutterInfoIcon, new TextAttributes(null, null, infoColor, EffectType.WAVE_UNDERSCORE, Font.PLAIN))
          } else {
            (null, new TextAttributes(null, infoColor, null, null, Font.PLAIN))
          }
        for (lTag <- lTags) (lTag: @unchecked) match {
          case tag: UriTag with LocationInfoTag with MessageTag with KindTag with SeverityTag =>
            val start = tag.offset
            val (ta, icon) = tag match {
              case _: InfoTag =>
                if (tag.kind == "hint") (null, gutterHintIcon)
                else (infoAttr, infoIcon)
              case _: WarningTag => (warningAttr, warningIcon)
              case _: ErrorTag | _: InternalError => (errorAttr, errorIcon)
            }
            val end = scala.math.min(tag.offset + tag.length, editor.getDocument.getTextLength)
            val rh = mm.addRangeHighlighter(start, end, 1000000, ta, HighlighterTargetArea.EXACT_RANGE)
            if (ta != null) {
              rh.setErrorStripeTooltip(tag.message)
              rh.setThinErrorStripeMark(false)
            }
            if (icon != null)
              rh.setGutterIconRenderer(new GutterIconRenderer {
                override def getIcon = icon

                override def getTooltipText =
                  if (tag.kind == "hint") "Click to show some hints"
                  else tag.message

                override def equals(other: Any) = false

                override def hashCode = System.identityHashCode(this)

                override def getClickAction =
                  if (tag.kind == "hint")
                    (e: AnActionEvent) =>
                      Option(SireumToolWindowFactory.windows.get(project)).
                        foreach(f => {
                          val tw = f.toolWindow.asInstanceOf[ToolWindowImpl]
                          tw.activate(
                            () => f.logika.logikaTextArea.setText(tag.message))
                        })
                  else null
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
    enableEditor(project, editor)
    analyze(project, editor, isSilent = false)
    e.getPresentation.setEnabled(true)
  }

  final override def update(e: AnActionEvent): Unit = {
    val project = e.getProject
    e.getPresentation.setEnabledAndVisible(project != null &&
      LogikaConfigurable.allFileExts.contains(Util.getFileExt(project)))
  }
}