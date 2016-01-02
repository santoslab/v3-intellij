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

import com.intellij.openapi.actionSystem._
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.util._
import org.sireum.intellij.SireumApplicationComponent

object LogikaCheckAction {
  val icon = IconLoader.getIcon("/logika.png")
  val fileExts = Set(".sc", ".scala", ".logika", ".lgk")

  def apply(): Unit = {
    new LogikaCheckAction

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

  def getFileExt(project: Project): String = {
    val fem = FileEditorManager.getInstance(project)
    fem.getSelectedFiles match {
      case Array(f, _*) =>
        val name = f.getName
        val i = name.lastIndexOf('.')
        if (i >= 0)
          name.substring(name.lastIndexOf('.'))
        else ""
      case _ => ""
    }
  }

  override def actionPerformed(e: AnActionEvent): Unit = {
    e.getPresentation.setEnabled(false)
    val input = Some(FileEditorManager.getInstance(e.getProject).
      getSelectedTextEditor.getDocument.getText)
    val isProgramming =
      getFileExt(e.getProject) match {
        case ".scala" | ".sc" => true
        case _ => false
      }
    SireumApplicationComponent.runSireum(
      e.getProject, input, "logika",
      if (isProgramming) "--ideprog" else "--ide") match {
      case Some(s) =>
        println(s)
      case _ =>
    }

    e.getPresentation.setEnabled(true)
  }
}