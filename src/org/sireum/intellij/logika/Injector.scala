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

package org.sireum.intellij.logika

import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector

object Injector {
  val enumAnnotation = "org.sireum.logika.enum"
  val recordAnnotations = Set("org.sireum.logika.record", "org.sireum.logika.irecord")
}

import Injector._

class Injector extends SyntheticMembersInjector {
  override def injectSupers(source: ScTypeDefinition): Seq[String] = {
    if (source.isObject &&
      source.getAnnotations().exists(a => enumAnnotation == a.getQualifiedName)) {
      Seq("Enumeration")
    } else Seq()
  }

  override def injectFunctions(source: ScTypeDefinition): Seq[String] = {
    if (source.isCase &&
      source.getConstructors.length == 1 &&
      source.getAnnotations.exists(a => recordAnnotations.contains(a.getQualifiedName))) {
      val c = source.getConstructors.head
      val params = c.getParameterList.getParameters
      if (params.nonEmpty) {
        val sb = new StringBuilder
        sb.append("def apply(")
        if (params.nonEmpty) {
          sb.append(params.head.getName)
          sb.append(": ")
          sb.append(params.head.getType.getPresentableText)
          sb.append(" = ")
          sb.append(params.head.getName)
          for (param <- params.tail) {
            sb.append(", ")
            sb.append(param.getName)
            sb.append(": ")
            sb.append(param.getType.getPresentableText)
            sb.append(" = ")
            sb.append(param.getName)
          }
        }
        sb.append("): ")
        sb.append(source.getName)
        sb.append(" =  ???")
        Seq(s"override def clone: ${source.getName} = ???", sb.toString)
      } else Seq(s"override def clone: ${source.getName} = ???")
    } else Seq()
  }
}