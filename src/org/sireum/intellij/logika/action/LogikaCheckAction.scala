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

import java.awt.{Color, Font}
import java.util.concurrent._

import com.intellij.openapi.editor.event._

import com.intellij.notification.{NotificationType, Notification, Notifications}
import com.intellij.openapi.actionSystem._
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.markup._
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.util._
import com.intellij.ui.JBColor
import org.sireum.intellij.SireumApplicationComponent
import org.sireum.logika.message._
import org.sireum.util._

object LogikaCheckAction {
  val icon = IconLoader.getIcon("/logika.png")
  val gutterErrorIcon = IconLoader.getIcon("/logika-error.png")
  val gutterWarningIcon = IconLoader.getIcon("/logika-warning.png")
  val gutterInfoIcon = IconLoader.getIcon("/logika-info.png")
  val fileExts = Set(".sc", ".scala", ".logika", ".lgk")
  val queue = new LinkedBlockingQueue[String]()
  val editorMap = mmapEmpty[String, (Project, Editor)]
  val idle = 2000
  var request: Option[(Long, String)] = None
  var processInit = false
  var terminated = false

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
            Message.unpickleOutput[OutputMessage](s) match {
              case r: Result => processResult(r)
            }
          }, "logika", "--ide")
      val t = new Thread {
        override def run(): Unit = {
          while (!terminated) {
            this.synchronized {
              request match {
                case Some((time, r)) =>
                  if (System.currentTimeMillis - time > idle) {
                    request = None
                    queue.add(r)
                  }
                case None =>
              }
            }
            Thread.sleep(idle / 2)
          }
        }
      }
      t.setDaemon(true)
      t.start()
    }
  }

  def getFilePath(project: Project): Option[String] = {
    val fem = FileEditorManager.getInstance(project)
    fem.getSelectedFiles match {
      case Array(f, _*) => Some(f.getCanonicalPath)
      case _ => None
    }
  }

  def getFileExt(project: Project): String = {
    getFilePath(project) match {
      case Some(path) =>
        val i = path.lastIndexOf('.')
        if (i >= 0)
          path.substring(path.lastIndexOf('.'))
        else ""
      case _ => ""
    }
  }

  def analyze(project: Project, editor: Editor, isSilent: Boolean): Unit = {
    val input = editor.getDocument.getText
    val isProgramming =
      getFileExt(project) match {
        case ".scala" | ".sc" => true
        case _ => false
      }
    val (t, requestId) =
      editorMap.synchronized {
        val t = System.currentTimeMillis
        val id = t.toString
        editorMap(id) = (project, editor)
        request = None
        (t, id)
      }
    val proofs = ivector(ProofFile(Some(getFilePath(project).get), input))
    val r = Message.pickleInput(Check(requestId, isSilent,
      isProgramming, proofs, lastOnly = false,
      autoEnabled = false, timeout = 2000, checkSat = false))
    if (isSilent)
      this.synchronized {
        request = Some((t, r))
      }
    else queue.add(r)
  }

  def editorOpened(project: Project, editor: Editor): Unit = {
    if (!fileExts.contains(getFileExt(project))) return
    init(project)
    editor.getDocument.addDocumentListener(new DocumentListener {
      override def documentChanged(event: DocumentEvent): Unit = {
        analyze(project, editor, isSilent = true)
      }

      override def beforeDocumentChange(event: DocumentEvent): Unit = {
      }
    })
    editor.addEditorMouseMotionListener(new EditorMouseMotionListener {
      override def mouseMoved(e: EditorMouseEvent): Unit = {
        if (!EditorMouseEventArea.EDITING_AREA.equals(e.getArea))
          return
        val component = editor.getContentComponent
        val point = e.getMouseEvent.getPoint
        val pos = editor.xyToLogicalPosition(point)
        val offset = editor.logicalPositionToOffset(pos)
        val rhs = editor.getMarkupModel.getAllHighlighters
        for (rh <- rhs) {
          rh match {
            case rh: RangeHighlighter if rh.getTextAttributes.isInstanceOf[LogikaTextAttributes] =>
              if (rh.getStartOffset <= offset && offset <= rh.getEndOffset) {
                component.setToolTipText(rh.getErrorStripeTooltip.toString)
                return
              }
            case _ =>
          }
        }
        component.setToolTipText(null)
      }

      override def mouseDragged(e: EditorMouseEvent): Unit = {
      }
    })
  }

  def notifyHelper(project: Project, tags: ISeq[Tag]): Unit = {
    def notify(n: Notification): Unit =
      new Thread() {
        override def run(): Unit = {
          Notifications.Bus.notify(n, project)
          Thread.sleep(5000)
          ApplicationManager.getApplication.invokeLater(new Runnable {
            override def run(): Unit = {
              n.expire()
            }
          })
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
      notify(new Notification(
        "Sireum Logika", "Logika Information",
        inlTags.map(_.asInstanceOf[MessageTag].message).mkString(lineSep),
        NotificationType.INFORMATION, null))
    }
  }

  def processResult(r: Result): Unit = {
    val tags = r.tags
    val (project, editor) = editorMap.synchronized {
      editorMap(r.requestId)
    }
    ApplicationManager.getApplication.invokeLater(new Runnable {
      override def run(): Unit = {
        val mm = editor.getMarkupModel
        for (h <- mm.getAllHighlighters) {
          h.getTextAttributes match {
            case ErrorTextAttributes | WarningTextAttributes | InfoTextAttributes =>
              mm.removeHighlighter(h)
            case _ =>
          }
        }
      }
    })
    val (lTags, nlTags) = tags.partition(_.isInstanceOf[UriTag with LocationInfoTag with MessageTag])
    if (!r.isSilent) notifyHelper(project, nlTags)
    if (lTags.isEmpty) return
    ApplicationManager.getApplication.invokeLater(new Runnable {
      override def run(): Unit = {
        if (!editor.isDisposed) {
          val mm = editor.getMarkupModel
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
          }
        }
      }
    })
  }
}

import LogikaCheckAction._

private class LogikaCheckAction
  extends AnAction("Check Logika Proof",
    "Check Logika Proof", LogikaCheckAction.icon) {

  // init
  {

    val am = ActionManager.getInstance

    val editorPopupGroup = am.getAction(IdeActions.GROUP_EDITOR_POPUP).
      asInstanceOf[DefaultActionGroup]
    editorPopupGroup.addAction(this, Constraints.LAST)

    val runGroup = am.getAction("SireumActionGroup").
      asInstanceOf[DefaultActionGroup]
    runGroup.addAction(this, Constraints.LAST)
  }

  override def update(e: AnActionEvent): Unit = {
    val project = e.getProject
    e.getPresentation.setVisible(project != null &&
      fileExts.contains(getFileExt(project)))
  }


  override def actionPerformed(e: AnActionEvent): Unit = {
    e.getPresentation.setEnabled(false)
    val project = e.getProject
    val editor = FileEditorManager.
      getInstance(project).getSelectedTextEditor
    if (editor == null) return
    analyze(project, editor, isSilent = false)
    e.getPresentation.setEnabled(true)
  }
}