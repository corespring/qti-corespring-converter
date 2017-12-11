package org.corespring.conversion.qti

import java.io.File
import java.net.URL
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipFile}

import org.apache.commons.io.IOUtils
import org.slf4j.LoggerFactory
import org.specs2.mutable.Specification
import play.api.libs.json.Json

import scala.collection.JavaConversions._
import scala.sys.process._


object RunHelper {

  val logger = LoggerFactory.getLogger(RunHelper.this.getClass)


  def mkTmpDir(prefix:String = "run-helper") = Files.createTempDirectory(s"$prefix")

  def buildZip(dir:Path, sourceId:String, cwd: URL): Path = {
    val out = dir.resolve(s"$sourceId.zip")
    val cmd = Seq("zip", "-r", out.toAbsolutePath.toString, ".")
    val code = Process(cmd, new File(cwd.toURI)).!
    logger.info(s">> cmd: $cmd, exitCode: $code")
    out
  }

  def run(input:String,
          output:String,
          vendor: String,
          sourceId:Option[String],
          metadata: String = "{}") = {
    val args = Array(
      "--input", input,
      "--vendor", vendor,
      "--limit", "0",
      "--output", output,
      "--killRuntime", "false",
      "--metadata", metadata) ++ sourceId.map(id => Array("--sourceId", id)).getOrElse(Array.empty)
        //"""{"scoringType": "SBAC"}"""
    Runner.main(args)
  }
}

trait BaseRunnerUtils {

  def json(zip: ZipFile, e: ZipEntry) = {
    val jsonString = IOUtils.toString(zip.getInputStream(e))
    Json.parse(jsonString)
  }

}
trait BaseRunner extends Specification with BaseRunnerUtils{

  def sourceId: String

  def vendor: String = "kds"

  protected val logger = LoggerFactory.getLogger(this.getClass)

  val tmpDir = RunHelper.mkTmpDir(s"sbac-runner-test-$sourceId")


  val sbacOutput = tmpDir.resolve(s"sbac-output-$sourceId.zip")


  val zippedPath = RunHelper.buildZip(
    tmpDir,
    sourceId,
    this.getClass().getResource(s"/$sourceId"))

  logger.info(s"zippedPath:  $zippedPath")
  println(s"zippedPath:  $zippedPath")

  val pathToSbac = zippedPath.toAbsolutePath.toString

  logger.info(s"sbacOutput: $sbacOutput")


  RunHelper.run(
    pathToSbac,
    sbacOutput.toString,
    vendor,
    Some(sourceId),
    """{"scoringType": "SBAC"}"""
  )


  val zip = new ZipFile(new File(sbacOutput.toString))

  println(s">>>> ${zip.entries().map(_.getName)}")

  val playerDef = zip.entries.find {
    e =>
      logger.info(s"e.getName: ${e.getName}")
      e.getName.contains("player-definition.json")
  }


  val playerDefJson = playerDef.map(json(zip, _)).getOrElse {
    throw new RuntimeException("Not defined")
  }

  val profile = zip.entries.find {
    e => e.getName.contains("profile.json")
  }

}

