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

package org.sireum.intellij.logika

import java.awt.Color
import javax.swing.JComponent
import javax.swing.event.{DocumentEvent, DocumentListener}

import com.intellij.ide.util.PropertiesComponent
import com.intellij.openapi.options.Configurable
import com.intellij.openapi.util.IconLoader
import com.intellij.ui.JBColor
import org.sireum.util._

object LogikaConfigurable {
  private val syntaxHighlightingKey = "org.sireum.logika.highlighting"
  private val underwaveKey = "org.sireum.logika.underwave"
  private val backgroundAnalysisKey = "org.sireum.logika.background"
  private val idleKey = "org.sireum.logika.idle"
  private val timeoutKey = "org.sireum.logika.timeout"
  private val autoEnabledKey = "org.sireum.logika.auto"
  private val checkSatKey = "org.sireum.logika.checkSat"
  private[logika] var syntaxHighlighting = true
  private[logika] var underwave = true
  private[logika] var backgroundAnalysis = true
  private[logika] var idle: Natural = 1500
  private[logika] var timeout: Natural = 2000
  private[logika] var autoEnabled = false
  private[logika] var checkSat = false
  private val logo = IconLoader.getIcon("/logika/icon/logika-logo.png")

  def loadConfiguration(): Unit = {
    val pc = PropertiesComponent.getInstance
    syntaxHighlighting = pc.getBoolean(syntaxHighlightingKey, syntaxHighlighting)
    underwave = pc.getBoolean(underwaveKey, underwave)
    backgroundAnalysis = pc.getBoolean(backgroundAnalysisKey, backgroundAnalysis)
    idle = pc.getInt(idleKey, idle)
    timeout = pc.getInt(timeoutKey, timeout)
    autoEnabled = pc.getBoolean(autoEnabledKey, autoEnabled)
    checkSat = pc.getBoolean(checkSatKey, checkSat)
  }

  def saveConfiguration(): Unit = {
    val pc = PropertiesComponent.getInstance
    pc.setValue(syntaxHighlightingKey, syntaxHighlighting.toString)
    pc.setValue(underwaveKey, underwave.toString)
    pc.setValue(backgroundAnalysisKey, backgroundAnalysis.toString)
    pc.setValue(idleKey, idle.toString)
    pc.setValue(timeoutKey, timeout.toString)
    pc.setValue(autoEnabledKey, autoEnabled.toString)
    pc.setValue(checkSatKey, checkSat.toString)
  }

  def parseGe200(text: String): Option[Natural] =
    try {
      val n = text.toInt
      if (n < 200) None else Some(n)
    } catch {
      case _: Throwable => None
    }
}

import LogikaConfigurable._

final class LogikaConfigurable extends LogikaForm with Configurable {

  private var validIdle = true
  private var validTimeout = true
  private var fgColor: Color = _

  override def getDisplayName: String = "Logika"

  override def getHelpTopic: String = null

  override def isModified: Boolean =
    validIdle && validTimeout &&
      (highlightingCheckBox.isSelected != syntaxHighlighting ||
        underwaveCheckBox.isSelected != underwave ||
        backgroundCheckBox.isSelected != backgroundAnalysis ||
        idleTextField.getText != idle.toString ||
        timeoutTextField.getText != timeout.toString ||
        autoCheckBox.isSelected != autoEnabled ||
        checkSatCheckBox.isSelected != checkSat)

  override def createComponent(): JComponent = {
    def updateIdle() = {
      val text = idleTextField.getText
      validIdle = parseGe200(text).nonEmpty
      idleLabel.setForeground(if (validIdle) fgColor else JBColor.red)
      idleTextField.setToolTipText(if (validIdle) "OK" else "Must be at least 200.")
    }
    def updateTimeout() = {
      val text = timeoutTextField.getText
      validTimeout = parseGe200(text).nonEmpty
      timeoutLabel.setForeground(if (validTimeout) fgColor else JBColor.red)
      timeoutTextField.setToolTipText(if (validTimeout) "OK" else "Must be at least 200.")
    }

    logoLabel.setIcon(logo)

    LogikaConfigurable.loadConfiguration()
    reset()

    fgColor = idleLabel.getForeground

    backgroundCheckBox.addActionListener(_ => {
      idleLabel.setEnabled(backgroundCheckBox.isSelected)
      idleTextField.setEnabled(backgroundCheckBox.isSelected)
    })

    idleTextField.getDocument.addDocumentListener(new DocumentListener {
      override def insertUpdate(e: DocumentEvent): Unit = updateIdle()

      override def changedUpdate(e: DocumentEvent): Unit = updateIdle()

      override def removeUpdate(e: DocumentEvent): Unit = updateIdle()
    })

    timeoutTextField.getDocument.addDocumentListener(new DocumentListener {
      override def insertUpdate(e: DocumentEvent): Unit = updateTimeout()

      override def changedUpdate(e: DocumentEvent): Unit = updateTimeout()

      override def removeUpdate(e: DocumentEvent): Unit = updateTimeout()
    })

    logikaPanel
  }

  override def disposeUIResources(): Unit = {}

  override def apply(): Unit = {
    syntaxHighlighting = highlightingCheckBox.isSelected
    underwave = underwaveCheckBox.isSelected
    backgroundAnalysis = backgroundCheckBox.isSelected
    idle = parseGe200(idleTextField.getText).getOrElse(idle)
    timeout = parseGe200(timeoutTextField.getText).getOrElse(timeout)
    autoEnabled = autoCheckBox.isSelected
    checkSat = checkSatCheckBox.isSelected
    saveConfiguration()
  }

  override def reset(): Unit = {
    highlightingCheckBox.setSelected(syntaxHighlighting)
    underwaveCheckBox.setSelected(underwave)
    backgroundCheckBox.setSelected(backgroundAnalysis)
    idleTextField.setText(idle.toString)
    timeoutTextField.setText(timeout.toString)
    autoCheckBox.setSelected(autoEnabled)
    checkSatCheckBox.setSelected(checkSat)
  }
}
