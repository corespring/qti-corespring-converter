package org.corespring.conversion.qti

import java.io.File
import java.net.URL
import java.nio.file.{Files, Path, Paths}
import java.util.zip.{ZipEntry, ZipFile}

import org.apache.commons.io.IOUtils
import org.slf4j.LoggerFactory
import org.specs2.mutable.Specification
import play.api.libs.json.Json

import scala.collection.JavaConversions._
import scala.sys.process._


trait BaseRunner extends Specification {

  def sourceId: String

  val logger = LoggerFactory.getLogger(this.getClass)

  val tmpDir = Files.createTempDirectory(s"sbac-runner-test-$sourceId")


  def json(zip: ZipFile, e: ZipEntry) = {
    val jsonString = IOUtils.toString(zip.getInputStream(e))
    Json.parse(jsonString)
  }

  val sbacOutput = tmpDir.resolve(s"sbac-output-$sourceId.zip")

  def zipDir(url: URL): Path = {

    val out = tmpDir.resolve(s"$sourceId.zip")

    val cmd = Seq("zip", "-r", out.toAbsolutePath.toString, ".")
    val code = Process(cmd, new File(url.toURI)).!

    logger.info(s">> cmd: $cmd, exitCode: $code")
    out
  }

  val zippedPath = zipDir(this.getClass().getResource(s"/$sourceId"))

  logger.info(s"zippedPath:  $zippedPath")
  println(s"zippedPath:  $zippedPath")

  val pathToSbac = zippedPath.toAbsolutePath.toString //new File(this.getClass().getResource(s"/$sourceId.zip").toURI).getAbsolutePath


  logger.info(s"sbacOutput: $sbacOutput")
  println(s"sbacOutput: $sbacOutput")

  logger.info(s">>>>>>>>>>> step")
  println(s">>>>>>>>>>> step")

  Runner.main(Array(
    "--input", pathToSbac,
    "--vendor", "kds",
    "--limit", "0",
    "--sourceId", sourceId,
    "--output", sbacOutput.toString,
    "--killRuntime", "false",
    "--metadata",
    """{"scoringType": "SBAC"}"""
  ))

  val zip = new ZipFile(new File(sbacOutput.toString))

  val playerDef = zip.entries.find {
    e =>
      logger.info(s"e.getName: ${e.getName}")
      e.getName.contains("player-definition.json")
  }

  val profile = zip.entries.find {
    e => e.getName.contains("profile.json")
  }

}

