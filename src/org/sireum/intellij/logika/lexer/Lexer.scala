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

import java.awt.Font
import java.io.StringReader

import com.intellij.openapi.editor.{DefaultLanguageHighlighterColors, Editor}
import com.intellij.openapi.editor.markup.{HighlighterTargetArea, TextAttributes, RangeHighlighter}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Key
import org.antlr.v4.runtime.{ANTLRInputStream, Token}
import org.sireum.intellij.Util
import org.sireum.util._

object Lexer {
  final val syntaxHighlightingDataKey = new Key[ISeq[RangeHighlighter]]("Logika Highlighting Data")
  final val propJusts = Set("premise", "andi", "^i", "ande1",
    "^e1", "ande2", "^e2", "ori1", "Vi1", "ori2", "Vi2", "ore", "Ve",
    "impliesi", "impliese", "noti", "negi", "note", "nege", "bottome",
    "falsee", "Pbc")
  final val predJusts = Set("foralli", "alli", "Ai", "foralle", "alle", "Ae",
    "existsi", "somei", "Ei", "existse", "somee", "Ee")
  final val progJusts = Set("subst1", "subst2", "algebra", "auto")
  final val propOps = Set("not", "and", "^", "or", "V", "implies")
  final val predOps = Set("forall", "all", "A", "exists", "some", "E")
  final val progOps = Set("*", "/", "%", "+", "-", "+:", ":+", "<", "<=",
    "≤", ">", ">=", "≥", "=", "==", "!=", "≠")
  final val justAndOps = propJusts ++ predJusts ++ progJusts ++ propOps ++ predOps
  final val types = Set("B", "Z", "ZS")
  final val constants = Set("true", "T", "⊤", "false", "F")
  final val constantJusts = Set("_|_", "⊥")
  final val andJustFollow = Set("i", "e1", "e2")
  final val orJustFollow = Set("i1", "i2", "e")
  final val propIeJustFirst = Set("->", "→", "!", "~", "¬")
  final val ieJustFollow = Set("i", "e")
  final val keywords = Set("abstract", "case", "catch", "class", "def",
    "do", "else", "extends", "final", "finally", "for", "forSome",
    "if", "implicit", "import", "lazy", "macro", "match", "new",
    "null", "object", "override", "package", "private",
    "protected", "return", "sealed", "super", "this", "throw",
    "trait", "try", "type", "val", "var", "while", "with", "yield",
    "pre", "requires", "modifies", "post", "ensures", "fact")

  sealed trait LogikaHighlightingTextAttributes

  object FunTextAttributes
    extends TextAttributes(null, null, null, null, Font.ITALIC)
    with LogikaHighlightingTextAttributes

  def addSyntaxHighlighter(project: Project, editor: Editor): Unit = {
    val mm = editor.getMarkupModel
    var rhs = ivectorEmpty[RangeHighlighter]

    def add(t: Token, ta: TextAttributes): Unit =
      rhs :+= mm.addRangeHighlighter(t.getStartIndex, t.getStopIndex + 1,
        900000, ta, HighlighterTargetArea.EXACT_RANGE)

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

    val tokens: CSeq[Token] = {
      import scala.collection.JavaConversions._
      lexer.getAllTokens
    }
    val size = tokens.size
    val isProgramming = tokens.exists(_.getText == "import")
    def peek(i: Int, f: Token => Boolean): Boolean =
      if (i < size) f(tokens(i)) else false

    val cs = editor.getColorsScheme

    import DefaultLanguageHighlighterColors._
    val plainAttr = new TextAttributes(cs.getDefaultForeground, null, null, null, Font.PLAIN)
    val stringAttr = cs.getAttributes(STRING)
    val keywordAttr = cs.getAttributes(KEYWORD)
    val lineCommentAttr = cs.getAttributes(LINE_COMMENT)
    val blockCommentAttr = cs.getAttributes(BLOCK_COMMENT)
    val logikaAttr = new TextAttributes(lineCommentAttr.getForegroundColor, null, null, null, Font.PLAIN)
    val typeAttr = cs.getAttributes(CLASS_REFERENCE)
    val constantAttr = cs.getAttributes(NUMBER)
    val justOpAttr = cs.getAttributes(CONSTANT)
    val annAttr = cs.getAttributes(METADATA)

    val ext = Util.getFileExt(project)
    if (ext == "scala" || ext == "sc") {
      mm.addRangeHighlighter(0, editor.getDocument.getText.length,
        800000, plainAttr, HighlighterTargetArea.EXACT_RANGE)
    }

    import Antlr4LogikaLexer._

    var i = 0
    while (i < size) {
      val token = tokens(i)
      token.getType match {
        case ID if !isProgramming =>
          if (peek(i + 1, _.getText == "("))
            add(token, FunTextAttributes)
        case NUM =>
          add(token, constantAttr)
        case SSTRING =>
          add(token, stringAttr)
        case COMMENT =>
          add(token, blockCommentAttr)
        case Antlr4LogikaLexer.LINE_COMMENT =>
          add(token, lineCommentAttr)
        case _ =>
          val text = token.getText
          if (justAndOps.contains(text))
            add(token, justOpAttr)
          else if (types.contains(text))
            add(token, typeAttr)
          else if (keywords.contains(text))
            add(token, keywordAttr)
          else if (text == "l\"\"\"") {
            add(token, logikaAttr)
            if (i + 1 < size && tokens(i + 1).getText == "{")
              add(tokens(i + 1), logikaAttr)
          } else if (text == "\"\"\"") {
            add(token, logikaAttr)
            val tM1 = tokens(i - 1)
            if (tM1.getText == "}")
              add(tM1, logikaAttr)
          } else if (constantJusts.contains(text)) {
            if (peek(i + 1, _.getText == "e")) {
              add(token, justOpAttr)
              add(tokens(i + 1), justOpAttr)
              i += 1
            } else add(token, constantAttr)
          } else if (text == "&&" || text == "∧") {
            if (peek(i + 1,
              t => andJustFollow.contains(t.getText)) &&
              peek(i + 2, _.getType == NUM)) {
              add(token, justOpAttr)
              add(tokens(i + 1), justOpAttr)
              i += 1
            } else add(token, justOpAttr)
          } else if (text == "||" || text == "∨") {
            if (peek(i + 1,
              t => orJustFollow.contains(t.getText)) &&
              peek(i + 2, _.getType == NUM)) {
              add(token, justOpAttr)
              add(tokens(i + 1), justOpAttr)
              i += 1
            } else add(token, justOpAttr)
          } else if (propIeJustFirst.contains(text)) {
            if (peek(i + 1,
              t => ieJustFollow.contains(t.getText)) &&
              peek(i + 2, _.getType == NUM)) {
              add(token, justOpAttr)
              add(tokens(i + 1), justOpAttr)
              i += 1
            } else add(token, justOpAttr)
          } else if (text == "∀" || text == "∃") {
            if (peek(i + 1, t => ieJustFollow.contains(t.getText)) &&
              peek(i + 2, t => t.getType == ID || t.getType == NUM)) {
              add(token, justOpAttr)
              add(tokens(i + 1), justOpAttr)
              i += 1
            } else add(token, justOpAttr)
          } else if (text == "@") {
            add(token, annAttr)
            if (peek(i + 1, _.getType == ID)) {
              add(tokens(i + 1), annAttr)
              i += 1
            }
          } else if (text == "invariant") {
            if (peek(i - 1, _.getText == "{"))
              add(token, keywordAttr)
            else add(token, justOpAttr)
          } else if (text == "assume") {
            if (!peek(i + 1, _.getText == "("))
              add(token, justOpAttr)
          }
      }
      i += 1
    }
    editor.putUserData(syntaxHighlightingDataKey, rhs)
  }
}
