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

package org.sireum.intellij.logika.lexer

import java.awt.{Color, Font}
import java.io.StringReader

import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.markup.{HighlighterTargetArea, TextAttributes, RangeHighlighter}
import com.intellij.openapi.util.Key
import com.intellij.ui.JBColor
import com.intellij.util.ui.UIUtil
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream, Token}
import org.sireum.util._

object Lexer {
  val syntaxHighlightingDataKey = new Key[ISeq[RangeHighlighter]]("Logika Highlighting Data")
  val propJusts = Set("assume", "premise", "andi", "^i", "ande1",
    "^e1", "ande2", "^e2", "ori1", "Vi1", "ori2", "Vi2", "ore", "Ve",
    "impliesi", "impliese", "noti", "negi", "note", "nege", "bottome",
    "falsee", "Pbc")
  val predJusts = Set("foralli", "alli", "Ai", "foralle", "alle", "Ae",
    "existsi", "somei", "Ei", "existse", "somee", "Ee")
  val progJusts = Set("subst1", "subst2", "algebra", "auto", "invariant")
  val propOps = Set("not", "and", "^", "or", "V", "implies")
  val predOps = Set("forall", "all", "A", "exists", "some", "E")
  val progOps = Set("*", "/", "%", "+", "-", "+:", ":+", "<", "<=",
    "≤", ">", ">=", "≥", "=", "==", "!=", "≠")
  val types = Set("B", "Z", "ZS")
  val constants = Set("true", "T", "⊤", "false", "F")
  val constantJusts = Set("_|_", "⊥")
  val andJustFollow = Set("i", "e1", "e2")
  val orJustFollow = Set("i1", "i2", "e")
  val propIeJustFirst = Set("->", "→", "!", "~", "¬")
  val ieJustFollow = Set("i", "e")
  val keywords = Set("abstract", "case", "catch", "class", "def",
    "do", "else", "extends", "final", "finally", "for", "forSome",
    "if", "implicit", "import", "lazy", "macro", "match", "new",
    "null", "object", "override", "package", "private",
    "protected", "return", "sealed", "super", "this", "throw",
    "trait", "try", "type", "val", "var", "while", "with", "yield")

  val propJustDarkColor = new Color(0xb3, 0x89, 0xc5)
  val predJustDarkColor = new Color(0x98, 0x76, 0xaa)
  val progJustDarkColor = new Color(0x82, 0x48, 0x92)
  val propJustColor = progJustDarkColor
  val progJustColor = propJustDarkColor
  val typeDarkColor = new Color(0x49, 0x73, 0x70)
  val typeColor = new Color(0x43, 0xa9, 0xac)
  val constantDarkColor = new Color(0x68, 0x97, 0xbb)
  val constantColor = new Color(0, 0, 0xff)
  val stringDarkColor = new Color(0x6a, 0x87, 0x59)
  val stringColor = new Color(0, 0x80, 0)
  val logikaDarkColor = new Color(0x60, 0x60, 0x60)
  val logikaColor = new Color(0x90, 0x90, 0x90)
  val keywordDarkColor = new Color(0xcc, 0x78, 0x32)
  val keywordColor = new Color(0, 0, 0x80)
  val plainDarkColor = new Color(0xa9, 0xb7, 0xc6)
  val plainColor = new Color(0x00, 0x00, 0x00)

  def isDark = UIUtil.isUnderDarcula

  sealed trait LogikaHighlightingTextAttributes

  object PropJustTextAttributes
    extends TextAttributes(null, null, null, null, Font.BOLD)
    with LogikaHighlightingTextAttributes {
    override def getForegroundColor =
      if (isDark) propJustDarkColor else propJustColor
  }

  object PredJustTextAttributes
    extends TextAttributes(predJustDarkColor, null, null, null, Font.BOLD)
    with LogikaHighlightingTextAttributes

  object ProgJustTextAttributes
    extends TextAttributes(null, null, null, null, Font.BOLD)
    with LogikaHighlightingTextAttributes {
    override def getForegroundColor =
      if (isDark) progJustDarkColor else progJustColor
  }

  object PropOpTextAttributes
    extends TextAttributes(null, null, null, null, Font.PLAIN)
    with LogikaHighlightingTextAttributes {
    override def getForegroundColor =
      if (isDark) propJustDarkColor else propJustColor
  }

  object PredOpTextAttributes
    extends TextAttributes(predJustDarkColor, null, null, null, Font.PLAIN)
    with LogikaHighlightingTextAttributes

  object ProgOpTextAttributes
    extends TextAttributes(null, null, null, null, Font.BOLD)
    with LogikaHighlightingTextAttributes {
    override def getForegroundColor =
      if (isDark) plainDarkColor else plainColor
  }

  object TypeTextAttributes
    extends TextAttributes(JBColor.green, null, null, null, Font.PLAIN)
    with LogikaHighlightingTextAttributes {
    override def getForegroundColor =
      if (isDark) typeDarkColor else typeColor
  }

  object ConstantTextAttributes
    extends TextAttributes(JBColor.blue, null, null, null, Font.PLAIN)
    with LogikaHighlightingTextAttributes {
    override def getForegroundColor =
      if (isDark) constantDarkColor else constantColor
  }

  object StringTextAttributes
    extends TextAttributes(JBColor.blue, null, null, null, Font.PLAIN)
    with LogikaHighlightingTextAttributes {
    override def getForegroundColor =
      if (isDark) stringDarkColor else stringColor
  }

  object LogikaTextAttributes
    extends TextAttributes(JBColor.blue, null, null, null, Font.PLAIN)
    with LogikaHighlightingTextAttributes {
    override def getForegroundColor =
      if (isDark) logikaDarkColor else logikaColor
  }

  object FunTextAttributes
    extends TextAttributes(null, null, null, null, Font.ITALIC)
    with LogikaHighlightingTextAttributes

  object KeywordTextAttributes
    extends TextAttributes(null, null, null, null, Font.BOLD)
    with LogikaHighlightingTextAttributes {
    override def getForegroundColor =
      if (isDark) keywordDarkColor else keywordColor
  }

  object PlainTextAttributes
    extends TextAttributes(null, null, null, null, Font.PLAIN)
    with LogikaHighlightingTextAttributes {
    override def getForegroundColor =
      if (isDark) plainDarkColor else plainColor
  }

  def addSyntaxHighlighter(editor: Editor): Unit = {
    val mm = editor.getMarkupModel
    var rhs = ivectorEmpty[RangeHighlighter]
    def add(t: Token, ta: TextAttributes): Unit = {
      rhs :+= mm.addRangeHighlighter(t.getStartIndex, t.getStopIndex + 1,
        900000, ta, HighlighterTargetArea.EXACT_RANGE)
    }

    Option(editor.getUserData(syntaxHighlightingDataKey)) match {
      case Some(prevRhs) =>
        for (rh <- prevRhs) {
          mm.removeHighlighter(rh)
        }
      case _ =>
    }
    val sr = new StringReader(editor.getDocument.getText)
    val inputStream = new ANTLRInputStream(sr)
    val lexer = new Antlr4LogikaLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    tokenStream.fill()
    import Antlr4LogikaLexer._
    val tokens: ISeq[Token] = {
      import scala.collection.JavaConversions._
      tokenStream.getTokens.filter(t =>
        t.getChannel == 0 || t.getType != NL).toVector
    }
    val size = tokens.size

    def peek(i: Int, f: Token => Boolean): Boolean =
      if (i < size) f(tokens(i)) else false

    mm.addRangeHighlighter(0, editor.getDocument.getText.length,
      800000, PlainTextAttributes, HighlighterTargetArea.EXACT_RANGE)
    var i = 0
    while (i < size) {
      val token = tokens(i)
      token.getType match {
        case ID =>
          if (peek(i + 1, _.getText == "("))
            add(token, FunTextAttributes)
        case NL =>
        case NUM =>
          add(token, ConstantTextAttributes)
        case SSTRING =>
          add(token, StringTextAttributes)
        case _ =>
          val text = token.getText
          if (propJusts.contains(text))
            add(token, PropJustTextAttributes)
          else if (predJusts.contains(text))
            add(token, PredJustTextAttributes)
          else if (progJusts.contains(text))
            add(token, ProgJustTextAttributes)
          else if (propOps.contains(text))
            add(token, PropOpTextAttributes)
          else if (predOps.contains(text))
            add(token, PredOpTextAttributes)
          else if (progOps.contains(text))
            add(token, ProgOpTextAttributes)
          else if (types.contains(text))
            add(token, TypeTextAttributes)
          else if (keywords.contains(text))
            add(token, KeywordTextAttributes)
          else if (text == "l\"\"\"") {
            add(token, LogikaTextAttributes)
            if (i + 1 < size && tokens(i + 1).getText == "{")
              add(tokens(i + 1), LogikaTextAttributes)
          } else if (text == "\"\"\"") {
            add(token, LogikaTextAttributes)
            val tM1 = tokens(i - 1)
            if (tM1.getText == "}")
              add(tM1, LogikaTextAttributes)
          } else if (constantJusts.contains(text)) {
            if (peek(i + 1, _.getText == "e")) {
              add(token, PropJustTextAttributes)
              add(tokens(i + 1), PropJustTextAttributes)
              i += 1
            } else add(token, ConstantTextAttributes)
          } else if (text == "&&" || text == "∧") {
            if (peek(i + 1,
              t => andJustFollow.contains(t.getText)) &&
              peek(i + 2, _.getType == NUM)) {
              add(token, PropJustTextAttributes)
              add(tokens(i + 1), PropJustTextAttributes)
              i += 1
            } else add(token, PropOpTextAttributes)
          } else if (text == "||" || text == "∨") {
            if (peek(i + 1,
              t => orJustFollow.contains(t.getText)) &&
              peek(i + 2, _.getType == NUM)) {
              add(token, PropJustTextAttributes)
              add(tokens(i + 1), PropJustTextAttributes)
              i += 1
            } else add(token, PropOpTextAttributes)
          } else if (propIeJustFirst.contains(text)) {
            if (peek(i + 1,
              t => ieJustFollow.contains(t.getText)) &&
              peek(i + 2, _.getType == NUM)) {
              add(token, PropJustTextAttributes)
              add(tokens(i + 1), PropJustTextAttributes)
              i += 1
            } else add(token, PropOpTextAttributes)
          } else if (text == "∀" || text == "∃") {
            if (peek(i + 1, t => ieJustFollow.contains(t.getText)) &&
              peek(i + 2, t => t.getType == ID || t.getType == NUM)) {
              add(token, PredJustTextAttributes)
              add(tokens(i + 1), PredJustTextAttributes)
              i += 1
            } else add(token, PredOpTextAttributes)
          }
      }
      i += 1
    }
    editor.putUserData(syntaxHighlightingDataKey, rhs)
  }
}
