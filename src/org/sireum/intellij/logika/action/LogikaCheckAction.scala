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
import java.awt.event.MouseEvent
import java.util.concurrent._
import javax.swing.{DefaultListModel, Icon, JSplitPane}

import com.intellij.openapi.editor.colors.{EditorFontType, TextAttributesKey}
import com.intellij.openapi.editor.event._
import com.intellij.notification.{Notification, NotificationType}
import com.intellij.openapi.actionSystem._
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.markup._
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.Messages
import com.intellij.openapi.ui.popup.{Balloon, JBPopupFactory}
import com.intellij.openapi.util._
import com.intellij.openapi.wm.StatusBarWidget.{IconPresentation, PlatformType, WidgetPresentation}
import com.intellij.openapi.wm.impl.ToolWindowImpl
import com.intellij.openapi.wm.{StatusBar, StatusBarWidget, WindowManager}
import com.intellij.ui.awt.RelativePoint
import com.intellij.util.Consumer
import com.intellij.util.ui.UIUtil
import org.sireum.intellij.{SireumApplicationComponent, SireumToolWindowFactory, Util}
import org.sireum.intellij.logika.{LogikaConfigurable, LogikaFileType}
import org.sireum.intellij.logika.lexer.Lexer
import org.sireum.logika.message._
import org.sireum.util._

object LogikaCheckAction {

  object EditorEnabled

  val icons: ISeq[Icon] = {
    var r = (0 to 6).map(n => IconLoader.getIcon(s"/logika/icon/logika-$n.png"))
    r = r.dropRight(1)
    r ++= r.reverse
    r = r.dropRight(1)
    r
  }
  val icon: Icon = icons.head
  val gutterErrorIcon: Icon = IconLoader.getIcon("/logika/icon/logika-gutter-error.png")
  val gutterWarningIcon: Icon = IconLoader.getIcon("/logika/icon/logika-gutter-warning.png")
  val gutterInfoIcon: Icon = IconLoader.getIcon("/logika/icon/logika-gutter-info.png")
  val gutterHintIcon: Icon = IconLoader.getIcon("/logika/icon/logika-gutter-hint.png")
  val gutterSummoningIcon: Icon = IconLoader.getIcon("/logika/icon/logika-gutter-summoning.png")
  val verifiedInfoIcon: Icon = IconLoader.getIcon("/logika/icon/logika-verified-info.png")
  val queue = new LinkedBlockingQueue[String]()
  val editorMap: MMap[String, (Project, Editor)] = mmapEmpty
  val logikaKey = new Key[EditorEnabled.type]("Logika")
  val analysisDataKey = new Key[ISeq[RangeHighlighter]]("Logika Analysis Data")
  val statusKey = new Key[Boolean]("Logika Analysis Status")
  var request: Option[Request] = None
  var processInit: Option[scala.sys.process.Process] = None
  var terminated = false
  var dividerWeight: Double = .2
  var tooltipMessageOpt: Option[String] = None
  var tooltipBalloonOpt: Option[Balloon] = None
  val tooltipDefaultBgColor: Color = new Color(0xff, 0xff, 0xcc)
  val tooltipDarculaBgColor: Color = new Color(0x5c, 0x5c, 0x42)

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
            override def getClickConsumer: Consumer[MouseEvent] = _ =>
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

  def analyze(project: Project, editor: Editor, isBackground: Boolean): Unit = {
    if (!isEnabled(editor)) return
    if (LogikaConfigurable.syntaxHighlighting)
      ApplicationManager.getApplication.invokeLater(
        (() => Lexer.addSyntaxHighlighter(project, editor)): Runnable,
        ((_: Any) => editor.isDisposed): Condition[Any])
    if (isBackground && !LogikaConfigurable.backgroundAnalysis) return
    init(project)
    val input = editor.getDocument.getText
    val proofs = ivector(ProofFile(Some(Util.getFilePath(project).get), input))
    val (t, requestId) = {
      val t = System.currentTimeMillis
      val id = t.toString
      (t, id)
    }

    def f: String = {
      Message.pickleInput(Check(
        requestId = requestId,
        isBackground = isBackground,
        kind = LogikaConfigurable.checkerKind,
        hintEnabled = LogikaConfigurable.hint,
        inscribeSummoningsEnabled = LogikaConfigurable.inscribeSummonings,
        coneInfluenceEnabled = LogikaConfigurable.coneInfluence,
        proofs = proofs,
        lastOnly = false,
        autoEnabled = LogikaConfigurable.autoEnabled,
        timeout = LogikaConfigurable.timeout,
        checkSatEnabled = LogikaConfigurable.checkSat,
        bitWidth = LogikaConfigurable.bitWidth,
        loopBound = LogikaConfigurable.loopBound,
        recursionBound = LogikaConfigurable.recursionBound,
        useMethodContract = LogikaConfigurable.methodContract))
    }

    if (isBackground) {
      this.synchronized {
        request = Some(Request(t, requestId, project, editor, f _))
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
      queue.add(f)
    }
  }


  def enableEditor(project: Project, editor: Editor): Unit = {
    if (editor.getUserData(logikaKey) != null) return
    editor.putUserData(logikaKey, EditorEnabled)
    editor.getDocument.addDocumentListener(new DocumentListener {
      override def documentChanged(event: DocumentEvent): Unit = {
        if (LogikaConfigurable.syntaxHighlighting || LogikaConfigurable.backgroundAnalysis)
          analyze(project, editor, isBackground = true)
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

  def editorOpened(project: Project, editor: Editor): Unit = {
    val ext = Util.getFileExt(project)
    if (!LogikaConfigurable.allFileExts.contains(ext)) return
    if (LogikaFileType.extensions.contains(ext)) {
      enableEditor(project, editor)
      editor.putUserData(statusKey, false)
      analyze(project, editor, isBackground = true)
    }
  }

  def notifyHelper(projectOpt: Option[Project], editorOpt: Option[Editor],
                   isBackground: Boolean, tags: ISeq[Tag]): Unit = {

    val project = projectOpt.orNull
    val statusOpt = editorOpt.map(_.getUserData(statusKey))
    val lineSep = scala.util.Properties.lineSeparator
    val ienlTags = tags.filter(_.isInstanceOf[InternalErrorTag])
    if (ienlTags.nonEmpty) {
      Util.notify(new Notification(
        "Sireum Logika", "Logika Internal Error",
        ienlTags.map(_.asInstanceOf[MessageTag].message).mkString(lineSep),
        NotificationType.ERROR), project, shouldExpire = true)
      editorOpt.foreach(_.putUserData(statusKey, false))
    }
    val enlTags = tags.filter(_.isInstanceOf[ErrorTag])
    if (enlTags.nonEmpty) {
      if (!isBackground || statusOpt.getOrElse(true))
        Util.notify(new Notification(
          "Sireum Logika", "Logika Error",
          enlTags.map(_.asInstanceOf[MessageTag].message).mkString(lineSep),
          NotificationType.ERROR), project, shouldExpire = true)
      editorOpt.foreach(_.putUserData(statusKey, false))
    }
    val wnlTags = tags.filter(_.isInstanceOf[WarningTag])
    if (wnlTags.nonEmpty && !isBackground) {
      Util.notify(new Notification(
        "Sireum Logika", "Logika Warning",
        wnlTags.map(_.asInstanceOf[MessageTag].message).mkString(lineSep),
        NotificationType.WARNING, null), project, shouldExpire = true)
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
      if (!isBackground || !(isVerified && statusOpt.getOrElse(false)))
        Util.notify(new Notification("Sireum Logika", title, msg,
          NotificationType.INFORMATION, null) {
          override def getIcon: Icon = icon
        }, project, shouldExpire = true)
    }
  }

  private sealed trait ReportItem

  private final case class ConsoleReportItem(fileResourceUri: FileResourceUri,
                                             line: PosInteger,
                                             column: PosInteger,
                                             offset: Natural,
                                             length: Natural,
                                             message: String) extends ReportItem {
    override val toString: String = s"[$line, $column] $message"
  }

  private final case class CheckSatReportItem(message: String) extends ReportItem

  private final case class HintReportItem(message: String) extends ReportItem

  private final case class SummoningReportItem(messageFirstLine: String,
                                               message: String) extends ReportItem {
    override def toString: String = messageFirstLine
  }

  private final case class ReportItems(error: MArray[ConsoleReportItem] = marrayEmpty,
                                       warning: MArray[ConsoleReportItem] = marrayEmpty,
                                       info: MArray[ConsoleReportItem] = marrayEmpty,
                                       var hint: Option[HintReportItem] = None,
                                       checksat: MArray[CheckSatReportItem] = marrayEmpty,
                                       summoning: MArray[SummoningReportItem] = marrayEmpty)

  private def processLocationTags(tags: IVector[Tag]): MMap[PosInteger, ReportItems] = {
    val reportItemMap: MMap[PosInteger, ReportItems] = scala.collection.mutable.TreeMap()

    for (tag <- tags) (tag: @unchecked) match {
      case tag: UriTag with LocationInfoTag with MessageTag with KindTag with SeverityTag =>
        val ris = reportItemMap.getOrElseUpdate(tag.lineBegin, ReportItems())
        tag match {
          case _: ErrorTag =>
            ris.error += ConsoleReportItem(tag.uri, tag.lineBegin, tag.columnBegin, tag.offset, tag.length, tag.message)
          case _: WarningTag if tag.kind == "checksat" =>
            ris.checksat += CheckSatReportItem(tag.message)
          case _: WarningTag =>
            ris.warning += ConsoleReportItem(tag.uri, tag.lineBegin, tag.columnBegin, tag.offset, tag.length, tag.message)
          case _: InfoTag if tag.kind == "hint" =>
            ris.hint = Some(HintReportItem(tag.message))
          case _: InfoTag if tag.kind == "summoning" =>
            val firstLine = tag.message.substring(tag.message.indexOf(';') + 1,
              tag.message.indexOf('\n')).trim
            ris.summoning += SummoningReportItem(firstLine, tag.message)
          case _: InfoTag =>
            ris.info += ConsoleReportItem(tag.uri, tag.lineBegin, tag.columnBegin, tag.offset, tag.length, tag.message)
        }
    }

    reportItemMap
  }

  def gutterIconRenderer(tooltipText: String, icon: Icon, action: AnAction): GutterIconRenderer =
    new GutterIconRenderer {
      override val getTooltipText: String = tooltipText

      override def getIcon: Icon = icon

      override def equals(other: Any): Boolean = false

      override def hashCode: Int = System.identityHashCode(this)

      override def getClickAction: AnAction = action
    }

  def saveSetDividerLocation(divider: JSplitPane, weight: Double): Unit = {
    val w = divider.getResizeWeight
    if (w != 0.0 && w != 1.0) {
      dividerWeight = w
    }
    divider.setDividerLocation(weight)
  }

  def sireumToolWindowFactory(project: Project,
                              g: SireumToolWindowFactory.Forms => Unit): Unit =
    Option(SireumToolWindowFactory.windows.get(project)).foreach(g)

  def toASCII(message: String): String = {
    val sb = new StringBuilder
    for (c <- message) c match {
      case '⊢' => sb.append("|-")
      case '∧' => sb.append("^")
      case '∨' => sb.append(" V ")
      case '¬' => sb.append("~")
      case '→' => sb.append("->")
      case '∀' => sb.append(" A ")
      case '∃' => sb.append(" E ")
      case '⊤' => sb.append(" T ")
      case '⊥' => sb.append("_|_")
      case '≤' => sb.append("<=")
      case '≥' => sb.append(">=")
      case '≠' => sb.append("!=")
      case _ => sb.append(c)
    }
    sb.replaceAllLiterally("  ", " ")
    sb.toString
  }

  def processResult(r: Result): Unit =
    ApplicationManager.getApplication.invokeLater(() => analysisDataKey.synchronized {
      val tags = r.tags
      if (r.requestId == "") {
        editorMap.synchronized {
          editorMap.clear()
        }
        notifyHelper(None, None, isBackground = false, tags)
        return
      }
      val (project, editor) = editorMap.synchronized {
        editorMap.get(r.requestId) match {
          case Some(pe) =>
            editorMap -= r.requestId
            pe
          case _ =>
            notifyHelper(None, None, isBackground = false, tags)
            return
        }
      }
      if (!editor.isDisposed) {
        sireumToolWindowFactory(project, f => {
          f.logika.logikaTextArea.setFont(
            editor.getColorsScheme.getFont(EditorFontType.PLAIN))
          f.logika.logikaTextArea.setText("")
        })
        editor.getContentComponent.setToolTipText(null)
        val mm = editor.getMarkupModel
        var rhs = editor.getUserData(analysisDataKey)
        if (rhs != null)
          for (rh <- rhs)
            mm.removeHighlighter(rh)
        editor.putUserData(analysisDataKey, null)
        val (lTags, nlTags) = tags.partition(
          _.isInstanceOf[UriTag with LocationInfoTag with MessageTag with KindTag with SeverityTag])
        notifyHelper(Some(project), Some(editor), r.isBackground, nlTags)
        if (lTags.isEmpty) return
        rhs = ivectorEmpty[RangeHighlighter]
        val cs = editor.getColorsScheme
        val errorColor = cs.getAttributes(
          TextAttributesKey.find("ERRORS_ATTRIBUTES")).getErrorStripeColor
        val (errorIcon, errorAttr) =
          if (LogikaConfigurable.underwave) {
            (gutterErrorIcon, new TextAttributes(null, null, errorColor, EffectType.WAVE_UNDERSCORE, Font.PLAIN))
          } else {
            (gutterErrorIcon, new TextAttributes(null, errorColor, null, null, Font.PLAIN))
          }
        errorAttr.setErrorStripeColor(errorColor)
        val warningColor = cs.getAttributes(TextAttributesKey.find("WARNING_ATTRIBUTES")).getErrorStripeColor
        val (warningIcon, warningAttr) =
          if (LogikaConfigurable.underwave) {
            (gutterWarningIcon, new TextAttributes(null, null, warningColor, EffectType.WAVE_UNDERSCORE, Font.PLAIN))
          } else {
            (gutterWarningIcon, new TextAttributes(null, warningColor, null, null, Font.PLAIN))
          }
        warningAttr.setErrorStripeColor(warningColor)
        val infoColor = cs.getAttributes(TextAttributesKey.find("TYPO")).getEffectColor
        val (infoIcon, infoAttr) =
          if (LogikaConfigurable.underwave) {
            (gutterInfoIcon, new TextAttributes(null, null, infoColor, EffectType.WAVE_UNDERSCORE, Font.PLAIN))
          } else {
            (gutterInfoIcon, new TextAttributes(null, infoColor, null, null, Font.PLAIN))
          }
        val layer = 1000000
        val tooltipSep = "<hr>"
        val listModel = new DefaultListModel[Object]()

        def consoleReportItems(cis: Iterable[ConsoleReportItem],
                               line: PosInteger, icon: Icon, attr: TextAttributes, color: Color): Unit = {
          val rhLine = mm.addLineHighlighter(line - 1, layer, null)
          rhLine.setThinErrorStripeMark(false)
          rhLine.setErrorStripeMarkColor(color)
          rhLine.setGutterIconRenderer(gutterIconRenderer(cis.map(_.message).mkString(tooltipSep),
            icon, _ => sireumToolWindowFactory(project, f => {
              val tw = f.toolWindow.asInstanceOf[ToolWindowImpl]
              tw.activate(() => {
                saveSetDividerLocation(f.logika.logikaToolSplitPane, 1.0)
                f.logika.logikaList.setModel(listModel)
              })
            })))
          rhs :+= rhLine
          for (ci <- cis) {
            listModel.addElement(ci)
            val end = scala.math.min(ci.offset + ci.length, editor.getDocument.getTextLength)
            try {
              val rh = mm.addRangeHighlighter(ci.offset, end, layer, attr, HighlighterTargetArea.EXACT_RANGE)
              rh.setErrorStripeTooltip(ci.message)
              rh.setThinErrorStripeMark(false)
              rh.setErrorStripeMarkColor(color)
              rhs :+= rh
            } catch {
              case _: IndexOutOfBoundsException | _: IllegalArgumentException =>
            }
          }
        }

        for ((line, ris) <- processLocationTags(lTags)) {
          if (ris.error.nonEmpty) consoleReportItems(ris.error, line, errorIcon, errorAttr, errorColor)
          if (ris.warning.nonEmpty) consoleReportItems(ris.warning, line, warningIcon, warningAttr, warningColor)
          if (ris.info.nonEmpty) consoleReportItems(ris.info, line, infoIcon, infoAttr, infoColor)
          if (ris.checksat.nonEmpty) {
            val rhLine = mm.addLineHighlighter(line - 1, layer, null)
            rhLine.setThinErrorStripeMark(false)
            rhLine.setGutterIconRenderer(gutterIconRenderer(ris.checksat.map(_.message).mkString(tooltipSep),
              warningIcon, null))
            rhs :+= rhLine
          }
          ris.hint match {
            case Some(hint) =>
              val rhLine = mm.addLineHighlighter(line - 1, layer, null)
              rhLine.setThinErrorStripeMark(false)
              rhLine.setGutterIconRenderer(gutterIconRenderer("Click to show some hints",
                gutterHintIcon, _ => sireumToolWindowFactory(project, f => {
                  val tw = f.toolWindow.asInstanceOf[ToolWindowImpl]
                  val font = editor.getColorsScheme.getFont(EditorFontType.PLAIN)
                  val isMeslo = font.getFontName.contains("Meslo")
                  f.logika.logikaTextArea.setFont(font)
                  val msg =
                    if (SystemInfo.isWindows && !isMeslo) toASCII(hint.message)
                    else hint.message
                  tw.activate(() => {
                    saveSetDividerLocation(f.logika.logikaToolSplitPane, 0.0)
                    f.logika.logikaTextArea.setText(msg)
                    f.logika.logikaTextArea.setCaretPosition(0)
                  })
                })
              ))
              rhs :+= rhLine
            case _ =>
          }
          if (ris.summoning.nonEmpty) {
            val rhLine = mm.addLineHighlighter(line - 1, layer, null)
            rhLine.setThinErrorStripeMark(false)
            rhLine.setGutterIconRenderer(gutterIconRenderer("Click to show scribed incantations",
              gutterSummoningIcon, _ => sireumToolWindowFactory(project, f => {
                val summoningListModel = new DefaultListModel[Object]()
                ris.summoning.foreach(summoningListModel.addElement)
                val tw = f.toolWindow.asInstanceOf[ToolWindowImpl]
                val font = editor.getColorsScheme.getFont(EditorFontType.PLAIN)
                f.logika.logikaTextArea.setFont(font)
                tw.activate(() => {
                  f.logika.logikaList.setModel(summoningListModel)
                  f.logika.logikaList.setSelectedIndex(0)
                })
              })))
            rhs :+= rhLine
          }
        }
        sireumToolWindowFactory(project, f => {
          f.logika.logikaTextArea.setFont(editor.getColorsScheme.getFont(EditorFontType.PLAIN))
          val list = f.logika.logikaList
          for (lsl <- list.getListSelectionListeners) {
            list.removeListSelectionListener(lsl)
          }
          list.setModel(listModel)
          list.addListSelectionListener(_ => {
            val i = list.getSelectedIndex
            if (0 <= i && i < list.getModel.getSize)
              list.getModel.getElementAt(i) match {
                case sri: SummoningReportItem =>
                  f.logika.logikaToolSplitPane.setDividerLocation(dividerWeight)
                  f.logika.logikaTextArea.setText(sri.message)
                  f.logika.logikaTextArea.setCaretPosition(0)
                case _ =>
              }
          })
          f.logika.logikaTextArea.setText("")
          saveSetDividerLocation(f.logika.logikaToolSplitPane, 1.0)
        })
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
    analyze(project, editor, isBackground = false)
    e.getPresentation.setEnabled(true)
  }

  final override def update(e: AnActionEvent): Unit = {
    val project = e.getProject
    e.getPresentation.setEnabledAndVisible(project != null &&
      LogikaConfigurable.allFileExts.contains(Util.getFileExt(project)))
  }
}