package org.corespring.conversion.qti

import java.io.File
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipFile}

import org.apache.commons.io.IOUtils
import org.slf4j.LoggerFactory
import org.specs2.mutable.Specification
import play.api.libs.json.Json

import scala.collection.JavaConversions._

class KdsRunnerSpec extends Specification {

  lazy val logger = LoggerFactory.getLogger(this.getClass)

  val tmpDir = Files.createTempDirectory("sbac-runner-test")

  val sbacOutput = tmpDir.resolve("sbac-output.zip")

  val pathToSbac = new File(this.getClass().getResource("/670508.zip").toURI).getAbsolutePath


  logger.info(s"sbacOutput: $sbacOutput")
  Runner.main(Array(
    "--input", pathToSbac,
    "--vendor", "kds",
    "--limit", "0",
    "--sourceId", "670508",
    "--output", sbacOutput.toString,
    "--killRuntime", "false",
    "--metadata",
    """{"scoringType": "SBAC"}"""
  ))

  def json(zip: ZipFile, e: ZipEntry) = {
    val jsonString = IOUtils.toString(zip.getInputStream(e))
    Json.parse(jsonString)
  }

  "kds --sourceId 670508" should {

    val zip = new ZipFile(new File(sbacOutput.toString))

    val playerDef = zip.entries.find {
      e =>
        logger.info(s"e.getName: ${e.getName}")
        e.getName.contains("player-definition.json")
    }

    val profile = zip.entries.find {
      e => e.getName.contains("profile.json")
    }

    val profileJson = profile.map(json(zip, _)).get

    "add scoringType to profile.json" in {
      (profileJson \ "taskInfo" \ "extended" \ "kds" \ "scoringType").as[String] must_== "SBAC"
    }

    "add sourceId to profile.json" in {
      (profileJson \ "taskInfo" \ "extended" \ "kds" \ "sourceId").as[String] must_== "670508"
    }

    "add title to profile.json" in {
      (profileJson \ "taskInfo" \ "title").as[String] must_== "670508 - SBAC"
    }

    "add description to profile.json" in {
      (profileJson \ "taskInfo" \ "description").as[String] must_== "670508 - SBAC"
    }

    "add corespring-number-line as the componentType for RESPONSE1" in {
      playerDef.map(json(zip, _)).map { json =>
        (json \ "components" \ "RESPONSE1" \ "componentType").as[String] must_== "corespring-number-line"
      }.getOrElse(ko)
    }

    "add the inline css" in {
      playerDef.map(json(zip, _)).map { json =>
        println(json \ "xhtml")
        (json \ "xhtml").as[String].contains(".qti.kds") must_== true
      }.getOrElse(ko)

    }
  }

}
