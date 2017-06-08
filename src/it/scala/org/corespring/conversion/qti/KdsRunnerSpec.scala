package org.corespring.conversion.qti

import java.io.File
import java.nio.file.{Files, Paths}
import java.util.zip.ZipFile

import org.apache.commons.io.IOUtils
import org.slf4j.LoggerFactory
import org.specs2.mutable.{Before, Specification}
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
    "--metadata", """{"scoringType": "SBAC"}"""
  ))

  "kds --sourceId 670508" should {


    "convert corespring-number-line" in {

      true must_== true

      val zip = new ZipFile(new File(sbacOutput.toString))
      val playerDef = zip.entries.find{
        e =>
          logger.info(s"e.getName: ${e.getName}")
          e.getName.contains("player-definition.json")
      }

      playerDef.map{ pd =>
        val jsonString = IOUtils.toString(zip.getInputStream(pd))
        val json = Json.parse(jsonString)
        logger.info(Json.prettyPrint(json))
        json
      }.map{json =>
        (json \ "components" \ "RESPONSE1" \ "componentType").as[String] must_== "corespring-number-line"
      }.getOrElse(ko)

    }
  }

}
