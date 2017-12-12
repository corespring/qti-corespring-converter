package org.corespring.conversion.qti

import org.corespring.macros.DescribeMacro.{describe => d}
import org.slf4j.LoggerFactory
import org.specs2.mutable.Specification
import play.api.libs.json.JsObject

class MeasuredProgressMultipleChoiceTest
  extends Specification
    with BaseRunnerUtils {



  val logger = LoggerFactory.getLogger(this.getClass)

  val sourceId = "MP-688"

  "measuredprogress " should {

    "add value to corespring-multiple-choice" in {

      val dir = RunHelper.mkTmpDir()

      val zip = RunHelper.buildZip(
        dir,
        sourceId,
        this.getClass.getResource(s"/MP-688"))


      val output = dir.resolve("new-mp.zip")

      //run the new converter
      RunHelper.run(
        zip.toAbsolutePath.toString,
        output.toString,
        "measuredprogress",
        Some(sourceId))

      val playerDefJson = loadFirstPlayerDefJson(output).get

      val m = (playerDefJson \ "components" \ "RESPONSE618" \ "model").as[JsObject]
      logger.info(d(m))
      (m \ "choices" \\ "value").map(_.as[String]) must_== Seq("SC-9831", "SC-9832", "SC-9833", "SC-9844")
    }
  }
}
