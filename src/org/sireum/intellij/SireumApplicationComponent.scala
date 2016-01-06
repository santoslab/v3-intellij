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

import java.io._
import java.util.concurrent.BlockingQueue

import com.intellij.ide.util.PropertiesComponent
import com.intellij.openapi.components._
import com.intellij.openapi.fileChooser._
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.Messages
import org.sireum.util._
import org.sireum.util.jvm.Exec

object SireumApplicationComponent {
  private val sireumHomeKey = "org.sireum"
  private val sireumVarArgsKey = "org.sireum.vmargs"
  private val sireumEnvVarsKey = "org.sireum.envvars"
  private[intellij] var sireumHome: Option[File] = None
  private[intellij] var vmArgs: ISeq[String] = ivectorEmpty
  private[intellij] var envVars = ilinkedMapEmpty[String, String]

  private var terminated: Boolean = false

  final def getSireumHome(project: Project = null): Option[File] = {
    val r =
      if (sireumHome.nonEmpty) sireumHome
      else {
        val env = System.getenv("SIREUM_HOME")
        sireumHome = checkSireumDir(env)
        if (sireumHome.isEmpty) {
          browseSireumHome(project).foreach(p =>
            sireumHome = checkSireumDir(p))
        }
        sireumHome
      }
    saveConfiguration()
    r
  }

  def sireumHomeString: String = sireumHome.map(_.getAbsolutePath).getOrElse("")

  def envVarsString: String = envVars.map(p => s"${p._1}=${p._2}").
    mkString(scala.util.Properties.lineSeparator)

  def vmArgsString: String = vmArgs.mkString(" ")

  def browseSireumHome(project: Project = null): Option[String] = {
    var pathOpt: Option[String] = None
    val desc = FileChooserDescriptorFactory.createSingleFolderDescriptor()
    desc.setTitle("Select Sireum v3 directory")
    FileChooser.chooseFile(
      desc,
      project, null, t => pathOpt = Some(t.getCanonicalPath))
    pathOpt.foreach(path =>
      if (checkSireumDir(path).isEmpty)
        Messages.showMessageDialog(project, sireumInvalid(path),
          "Invalid Sireum Configuration", null)
    )
    pathOpt
  }

  def sireumInvalid(path: String): String =
    s"""Could not confirm a working Sireum installation in $path (with the specified VM arguments and environment variables in the settings).
        |Make sure to run Sireum at least once from the command-line.""".stripMargin

  def runSireum(project: Project, input: Option[String], args: String*): Option[String] =
    getSireumHome(project) match {
      case Some(d) => runSireum(d.getAbsolutePath, vmArgs, envVars, input, args)
      case _ => None
    }

  def getSireumProcess(project: Project,
                       queue: BlockingQueue[String],
                       processOutput: String => Unit,
                       args: String*): Boolean =
    getSireumHome(project) match {
      case Some(d) =>
        val path = d.getAbsolutePath
        new Exec().process(Seq(s"$path/platform/java/bin/java", "-jar",
          s"$path/bin/sireum.jar") ++ args, { os =>
          try {
            val w = new OutputStreamWriter(os)
            val lineSep = scala.util.Properties.lineSeparator
            while (!terminated) {
              val m = queue.take()
              w.write(m)
              w.write(lineSep)
              w.flush()
            }
          } finally os.close()
        }, { is =>
          try {
            val r = new BufferedReader(new InputStreamReader(is))
            while (!terminated) {
              val line = r.readLine()
              if (line != null) {
                processOutput(line)
              }
            }
          } finally is.close()
        }, ("SIREUM_HOME", path))
        true
      case _ => false
    }

  private def runSireum(path: String,
                        vmArgs: ISeq[String],
                        envVars: ILinkedMap[String, String],
                        input: Option[String],
                        args: Seq[String]): Option[String] = {
    if (path.trim == "") None
    else new Exec().run(0,
      (s"$path/platform/java/bin/java" +: vmArgs) ++ Seq("-jar",
        s"$path/bin/sireum.jar") ++ args,
      input, envVars.toSeq :+("SIREUM_HOME", path): _*) match {
      case Exec.StringResult(s, _) => Some(s)
      case _ => None
    }
  }

  private[intellij] final def
  checkSireumDir(path: String,
                 vmArgs: ISeq[String] = this.vmArgs,
                 envVars: ILinkedMap[String, String] = this.envVars): Option[File] = {
    if (path == null) return None
    runSireum(path, vmArgs, envVars, None, Seq()) match {
      case Some(s) =>
        if (s.lines.exists(
          _.trim == "Sireum: A Software Analysis Platform (v3)")) {
          Some(new File(path))
        } else None
      case _ => None
    }
  }

  def parseEnvVars(text: String): Option[ILinkedMap[String, String]] = {
    var r = ilinkedMapEmpty[String, String]
    for (l <- text.split('\n')) {
      val kv = l.split('=')
      if (kv.length != 2) return None
      val Array(k, v) = kv
      if (k.charAt(0).isDigit) return None
      if (!k.forall(c => c == '_' || c.isLetterOrDigit)) return None
      r += k -> v
    }
    Some(r)
  }

  def parseVmArgs(text: String): Option[ISeq[String]] =
    if (text.trim == "") Some(ivectorEmpty)
    else {
      val r = text.split(' ').toVector
      if (r.forall(_.head == '-')) Some(r)
      else None
    }

  def loadConfiguration(): Unit = {
    val pc = PropertiesComponent.getInstance
    envVars = Option(pc.getValue(sireumEnvVarsKey)).flatMap(parseEnvVars).getOrElse(ilinkedMapEmpty)
    vmArgs = Option(pc.getValue(sireumVarArgsKey)).flatMap(parseVmArgs).getOrElse(ivectorEmpty)
    sireumHome = Option(pc.getValue(sireumHomeKey)).flatMap(p => checkSireumDir(p, vmArgs, envVars))
  }

  def saveConfiguration(): Unit = {
    val pc = PropertiesComponent.getInstance
    pc.setValue(sireumHomeKey, sireumHome.map(_.getAbsolutePath).orNull)
    pc.setValue(sireumEnvVarsKey, envVarsString)
    pc.setValue(sireumVarArgsKey, vmArgs.mkString(" "))
  }
}

class SireumApplicationComponent extends ApplicationComponent {

  override val getComponentName: String = "Sireum Application"

  override def initComponent(): Unit = {
    SireumApplicationComponent.loadConfiguration()
  }

  override def disposeComponent(): Unit = {
    SireumApplicationComponent.terminated = true
  }
}
