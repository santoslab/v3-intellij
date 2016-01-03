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

import java.util.concurrent.BlockingQueue

import com.intellij.ide.util.PropertiesComponent
import com.intellij.notification.{Notifications, NotificationsManager, Notification}
import com.intellij.openapi.components._
import java.io._
import com.intellij.openapi.fileChooser._
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.Messages
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.util.Consumer
import org.sireum.util.jvm.Exec

object SireumApplicationComponent {
  private var _sireumHome: Option[File] = None
  private var terminated: Boolean = false

  final def sireumHome(project: Project): Option[File] = {
    if (_sireumHome.nonEmpty) return _sireumHome
    val pc = PropertiesComponent.getInstance
    checkSireumDir(pc.getValue("org.sireum"))
    if (_sireumHome.nonEmpty) return _sireumHome
    val env = System.getenv("SIREUM_HOME")
    if (checkSireumDir(env).isEmpty) {
      var pathOpt: Option[String] = None
      val desc = FileChooserDescriptorFactory.createSingleFolderDescriptor()
      desc.setTitle("Select Sireum v3 directory")
      FileChooser.chooseFile(
        desc,
        project, null, new Consumer[VirtualFile] {
          override def consume(t: VirtualFile): Unit = {
            pathOpt = Some(t.getCanonicalPath)
          }
        })
      pathOpt.foreach(path =>
        if (checkSireumDir(path).isEmpty)
          Messages.showMessageDialog(project, "Sireum Not Found",
            s"""$path does not seem to contain a working Sireum installation.
               |Make sure to run Sireum at least once from the command-line.
                       """.stripMargin, null)
      )
    }
    _sireumHome
  }

  def runSireum(project: Project, input: Option[String], args: String*): Option[String] =
    sireumHome(project) match {
      case Some(d) => runSireum(d.getAbsolutePath, input, args)
      case _ => None
    }

  def getSireumProcess(project: Project,
                       queue: BlockingQueue[String],
                       processOutput: String => Unit,
                       args: String*): Boolean =
    sireumHome(project) match {
      case Some(d) =>
        val path = d.getAbsolutePath
        new Exec().process(Seq(s"$path/platform/java/bin/java", "-jar",
          s"$path/jvm/target/scala-2.11/sireum.jar") ++ args, { os =>
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

  private def runSireum(path: String, input: Option[String], args: Seq[String]): Option[String] = {
    new Exec().run(0,
      Seq(s"$path/platform/java/bin/java", "-jar",
        s"$path/jvm/target/scala-2.11/sireum.jar") ++ args,
      input, ("SIREUM_HOME", path)) match {
      case Exec.StringResult(s, _) => Some(s)
      case _ => None
    }
  }

  private final def checkSireumDir(path: String): Option[File] = {
    if (path == null) return None
    runSireum(path, None, Seq()) match {
      case Some(s) =>
        if (s.lines.exists(
          _.trim == "Sireum: A Software Analysis Platform (v3)")) {
          val pc = PropertiesComponent.getInstance
          pc.setValue("org.sireum", path)
          _sireumHome = Some(new File(path))
          _sireumHome
        } else None
      case _ => None
    }
  }
}

class SireumApplicationComponent extends ApplicationComponent {

  override val getComponentName: String = "Sireum Application"

  override def initComponent(): Unit = {
  }

  override def disposeComponent(): Unit = {
    SireumApplicationComponent.terminated = true
  }
}
