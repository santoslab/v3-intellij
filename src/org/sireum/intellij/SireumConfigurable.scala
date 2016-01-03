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

package org.sireum.intellij

import java.awt.Color
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JComponent
import javax.swing.event.{DocumentEvent, DocumentListener}

import com.intellij.openapi.options.Configurable
import com.intellij.ui.JBColor

import org.sireum.util._

final class SireumConfigurable extends SireumForm with Configurable {

  import SireumApplicationComponent._

  private var sireumHomeValue: String = _
  private var vmArgsValue: String = _
  private var envVarsValue: String = _
  private var validSireumHome = false
  private var validEnvVars = false
  private var fgColor: Color = _

  override def getDisplayName: String = "Sireum"

  override def getHelpTopic: String = null

  override def isModified: Boolean = {
    validSireumHome && validEnvVars &&
      (sireumHomeValue != sireumHomeTextField.getText ||
        vmArgsValue != vmArgsTextField.getText ||
        envVarsValue != envVarsTextArea.getText)
  }

  override def createComponent(): JComponent = {
    def updateSireumHome(path: String): Unit = {
      validSireumHome = checkSireumDir(path).nonEmpty
      sireumHomeLabel.setForeground(if (validSireumHome) fgColor else JBColor.red)
      sireumHomeTextField.setToolTipText(if (validSireumHome) "OK" else sireumInvalid(path))
    }
    def updateEnvVars(text: String): Unit = {
      validEnvVars = text == "" || parseEnvVars(text).nonEmpty
      envVarsLabel.setForeground(if (validEnvVars) fgColor else JBColor.red)
      envVarsLabel.setToolTipText(if (validSireumHome) "OK" else "Ill-formed (format: key of [a-zA-Z_][a-zA-Z0-9_]* = value, per line).")
    }

    load()

    reset()

    fgColor = sireumHomeLabel.getForeground

    sireumHomeTextField.getDocument.addDocumentListener(new DocumentListener {
      override def insertUpdate(e: DocumentEvent): Unit = update()

      override def changedUpdate(e: DocumentEvent): Unit = update()

      override def removeUpdate(e: DocumentEvent): Unit = update()

      def update(): Unit = updateSireumHome(sireumHomeTextField.getText)
    })

    sireumHomeButton.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit =
        browseSireumHome(null) match {
          case Some(p) =>
            updateSireumHome(p)
            if (validSireumHome) sireumHomeTextField.setText(p)
            else updateSireumHome(sireumHomeTextField.getText)
          case _ =>
        }
    })

    updateSireumHome(sireumHomeValue)

    envVarsTextArea.getDocument.addDocumentListener(new DocumentListener {
      override def insertUpdate(e: DocumentEvent): Unit = update()

      override def changedUpdate(e: DocumentEvent): Unit = update()

      override def removeUpdate(e: DocumentEvent): Unit = update()

      def update(): Unit = updateEnvVars(envVarsTextArea.getText.trim)
    })

    updateEnvVars(envVarsValue)

    sireumPanel
  }

  def load(): Unit = {
    sireumHomeValue = sireumHome.map(_.getAbsolutePath).getOrElse("")
    vmArgsValue = vmArgsString
    envVarsValue = envVarsString
  }

  override def disposeUIResources(): Unit = {
    fgColor = null
  }

  override def apply(): Unit = {
    sireumHome = checkSireumDir(sireumHomeTextField.getText)
    envVars = parseEnvVars(envVarsTextArea.getText).getOrElse(ilinkedMapEmpty)
    vmArgs = parseVmArgs(vmArgsTextField.getText)
    saveConfiguration()
    load()
  }

  override def reset(): Unit = {
    sireumHomeTextField.setText(sireumHomeValue)
    vmArgsTextField.setText(vmArgsValue)
    envVarsTextArea.setText(envVarsValue)
  }
}
