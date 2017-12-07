package org.corespring.utils

import java.io.{File, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import org.slf4j.LoggerFactory

import scala.sys.process._
import org.corespring.macros.DescribeMacro._

object ErrorDir{

  val logger = LoggerFactory.getLogger(ErrorDir.this.getClass)
  val home = {
    val uh = System.getProperty("user.home")
    if(uh != null) uh else {
      "echo ~".!!.trim
    }
  }


  val homeDir = Paths.get(new File(home).toURI)
  val errorPath = homeDir.resolve(".qti-corespring-converter/errors")

  logger.info(describe(homeDir, errorPath))

  def init = {
    Files.createDirectories(errorPath)
    require(Files.exists(errorPath) == true)
  }

  def remove = {
    val result = s"rm -fr ${errorPath.toAbsolutePath}".!!
    logger.info(describe(result))
    require(Files.exists(errorPath) == false)
    result
  }

  def dump(name:String, err: Option[Throwable], data: (String,String)*) = {
    logger.info(describe(name))
    val targetPath = errorPath.resolve(name)
    Files.createDirectories(targetPath)
    require(Files.exists(targetPath))
    data.foreach{ case(name, contents) => {
      Files.write(targetPath.resolve(name), contents.getBytes(StandardCharsets.UTF_8))
    }}
    err.foreach{ e =>
      val ps = new PrintStream(targetPath.resolve("error.log").toFile)
      e.printStackTrace(ps)
    }
  }
}
