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

import com.intellij.openapi.actionSystem.{ActionManager, AnActionEvent, Constraints, DefaultActionGroup}
import com.intellij.openapi.command.WriteCommandAction
import com.intellij.openapi.fileEditor.FileEditorManager
import org.sireum.intellij.Util
import org.sireum.intellij.logika.LogikaConfigurable
import org.sireum.logika.util.SymbolConverter

abstract class LogikaConvertSymbol extends LogikaAction {
  final val encodings: Set[String] = Set("UTF-8", "UTF8", "UTF-16", "UTF-32", "UTF_32")

  // init
  {
    val am = ActionManager.getInstance

    val runGroup = am.getAction("SireumLogikaGroup").
      asInstanceOf[DefaultActionGroup]
    runGroup.addAction(this, Constraints.LAST)
  }

  def isAscii: Boolean

  override def actionPerformed(e: AnActionEvent): Unit = {
    val project = e.getProject
    val editor = FileEditorManager.
      getInstance(project).getSelectedTextEditor
    if (editor == null) return
    val document = editor.getDocument
    val text = if (isAscii)
      SymbolConverter.toASCII(document.getText)
    else
      SymbolConverter.toUnicode(document.getText)

    WriteCommandAction.runWriteCommandAction(project,
      (() => document.setText(text)): Runnable)
  }

  override def update(e: AnActionEvent): Unit = {
    val project = e.getProject
    e.getPresentation.setEnabledAndVisible(
      project != null &&
        encodings.contains(System.getProperty("file.encoding")) &&
        LogikaConfigurable.allFileExts.contains(Util.getFileExt(project)))
  }
}

final class LogikaConvertAscii extends LogikaConvertSymbol {
  val isAscii: Boolean = true
}

final class LogikaConvertUnicode extends LogikaConvertSymbol {
  val isAscii: Boolean = false
}

