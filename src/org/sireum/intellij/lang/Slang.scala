/*
 Copyright (c) 2017, Robby, Kansas State University
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

package org.sireum.intellij.lang

import java.awt.Font
import java.io.File
import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.editor.event._
import com.intellij.openapi.editor.markup.{EffectType, HighlighterTargetArea, RangeHighlighter, TextAttributes}
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.popup.{Balloon, JBPopupFactory}
import com.intellij.openapi.util.{Condition, Key}
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.ui.awt.RelativePoint
import com.intellij.util.ui.UIUtil
import org.sireum.{ISZ, Some => SSome}
import org.sireum.intellij.Util
import org.sireum.intellij.logika.action.LogikaCheckAction
import org.sireum.intellij.logika.action.LogikaCheckAction._
import org.sireum.lang.parser.SlangParser
import org.sireum.lang.util.AccumulatingReporter
import org.sireum.lang.util.Reporter.Message.Level
import org.sireum.util._

object Slang {

  object EditorEnabled

  val changedKey = new Key[Option[Long]]("Slang Last Changed")
  val analysisDataKey = new Key[IVector[RangeHighlighter]]("Slang Analysis Data")

  val scheduler: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
  val changeThreshold = 1000

  val layer = 1000000
  val tooltipSep = "<hr>"
  val emptyAction: AnAction = _ => {}

  def editorOpened(project: Project, file: VirtualFile, editor: Editor): Unit = {
    val ext = Util.getFileExt(project)
    ext match {
      case "scala" | "slang" =>
        val fileUri = new File(file.getCanonicalPath).toURI.toString
        val (r, tags) = parse(editor.getDocument.getText, fileUri)
        if (r.hashSireum || ext == "slang") {
          processResult(editor, tags)
          editor.putUserData(changedKey, Some(System.currentTimeMillis()))
          scheduler.scheduleAtFixedRate((() => analyze(editor, fileUri)): Runnable, 0, changeThreshold, TimeUnit.MILLISECONDS)
          editor.getDocument.addDocumentListener(new DocumentListener {
            override def documentChanged(event: DocumentEvent): Unit = {
              if (editor.isDisposed) return
              editor.synchronized {
                editor.putUserData(changedKey, Some(System.currentTimeMillis()))
              }
            }

            override def beforeDocumentChange(event: DocumentEvent): Unit = {
            }
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
              editor.synchronized {
                tooltipMessageOpt match {
                  case Some(_) => tooltipMessageOpt = None
                  case _ =>
                }
                tooltipBalloonOpt match {
                  case Some(b) => b.hide(); b.dispose()
                  case _ =>
                }
              }
              var msgs: IVector[String] = ivectorEmpty
              for (rh <- rhs if rh.getErrorStripeTooltip != null)
                if (rh.getStartOffset <= offset && offset <= rh.getEndOffset) {
                  msgs :+= rh.getErrorStripeTooltip.toString
                }
              if (msgs.nonEmpty) {
                editor.synchronized {
                  tooltipMessageOpt = Some(msgs.mkString("<hr>"))
                }
                new Thread() {
                  override def run(): Unit = {
                    val tbo = editor.synchronized(tooltipMessageOpt)
                    Thread.sleep(500)
                    editor.synchronized {
                      if (tbo eq tooltipMessageOpt) tooltipMessageOpt match {
                        case Some(msg) =>
                          val color = if (UIUtil.isUnderDarcula) tooltipDarculaBgColor else tooltipDefaultBgColor
                          val builder = JBPopupFactory.getInstance().createHtmlTextBalloonBuilder(
                            msg, null, color, null)
                          val b = builder.createBalloon()
                          tooltipBalloonOpt = Some(b)
                          ApplicationManager.getApplication.invokeLater(
                            { () =>
                              b.show(new RelativePoint(component, point), Balloon.Position.below)
                            }: Runnable,
                            ((_: Any) => b.isDisposed): Condition[Any])
                        case _ =>
                      }
                    }
                  }
                }.start()
              }
            }

            override def mouseDragged(e: EditorMouseEvent): Unit = {}
          })

        }

      case _ =>
    }

  }

  def parse(text: String, fileUri: FileResourceUri): (SlangParser.Result, Seq[Tag]) = {
    val reporter = AccumulatingReporter(ISZ())
    val r = SlangParser(allowSireumPackage = "true" == System.getProperty("org.sireum.ive.dev"),
      isWorksheet = false, isDiet = false, fileUriOpt = SSome(fileUri), text = text, reporter)
    val fileUriOpt = Some(fileUri)
    var tags = Vector[Tag]()
    for (m <- reporter.messages) {
      val SSome(pos) = m.posOpt
      val li = LocationInfo(pos.beginLine.toInt, pos.beginColumn.toInt, pos.endLine.toInt,
        pos.endColumn.toInt, pos.offset.toInt, pos.length.toInt)
      m.level match {
        case Level.InternalError | Level.Error => tags :+= li.toLocationError(fileUriOpt, m.kind.value, m.message.value)
        case Level.Warning => tags :+= li.toLocationWarning(fileUriOpt, m.kind.value, m.message.value)
        case Level.Info => tags :+= li.toLocationInfo(fileUriOpt, m.kind.value, m.message.value)
      }
    }
    (r, tags)
  }

  def analyze(editor: Editor, fileUri: FileResourceUri): Unit = {
    editor.synchronized {
      editor.getUserData(changedKey) match {
        case Some(lastChanged) =>
          val d = System.currentTimeMillis() - lastChanged
          if (d > changeThreshold) {
            processResult(editor, parse(editor.getDocument.getText, fileUri)._2)
            editor.putUserData(changedKey, null)
          }
        case _ =>
      }
    }
  }

  def processResult(editor: Editor, tags: Seq[Tag]): Unit =
    ApplicationManager.getApplication.invokeLater(() => editor.synchronized {
      val mm = editor.getMarkupModel
      var rhs = editor.getUserData(analysisDataKey)
      if (rhs != null)
        for (rh <- rhs)
          mm.removeHighlighter(rh)
      rhs = ivectorEmpty[RangeHighlighter]
      val cs = editor.getColorsScheme
      val errorColor = cs.getAttributes(
        TextAttributesKey.find("ERRORS_ATTRIBUTES")).getErrorStripeColor
      val (errorIcon, errorAttr) =
        (gutterErrorIcon, new TextAttributes(null, null, errorColor, EffectType.WAVE_UNDERSCORE, Font.PLAIN))
      errorAttr.setErrorStripeColor(errorColor)
      var lineMap = Map[Int, IVector[FileLocationInfoErrorMessage]]()
      for (tag <- tags) (tag: @unchecked) match {
        case tag: FileLocationInfoErrorMessage =>
          lineMap += tag.lineBegin -> (lineMap.getOrElse(tag.lineBegin, ivectorEmpty) :+ tag)
          val end = scala.math.min(tag.offset + tag.length, editor.getDocument.getTextLength)
          val rh = mm.addRangeHighlighter(tag.offset, end, layer, errorAttr, HighlighterTargetArea.EXACT_RANGE)
          rh.setErrorStripeTooltip(tag.message)
          rh.setThinErrorStripeMark(false)
          rh.setErrorStripeMarkColor(errorColor)
          rhs :+= rh
      }
      for ((line, tags) <- lineMap) {
        val rhLine = mm.addLineHighlighter(line - 1, layer, null)
        rhLine.setThinErrorStripeMark(false)
        rhLine.setErrorStripeMarkColor(errorColor)
        rhLine.setGutterIconRenderer(
          LogikaCheckAction.gutterIconRenderer(tags.map(_.message).mkString(tooltipSep),
            errorIcon, emptyAction))
        rhs :+= rhLine
      }
      editor.putUserData(analysisDataKey, rhs)
    })

  def editorClosed(project: Project, file: VirtualFile): Unit = {}
}
