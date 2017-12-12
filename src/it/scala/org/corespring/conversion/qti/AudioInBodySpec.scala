package org.corespring.conversion.qti

import org.specs2.mutable.Specification
import play.api.libs.json.Json
import org.corespring.macros.DescribeMacro.{describe => d}
import org.slf4j.LoggerFactory

class AudioInBodySpec extends Specification with BaseRunnerUtils {

  private val logger = LoggerFactory.getLogger(this.getClass)
  "runner" should {

    "converts audio in CDATA" in {
      val audioInCData = """<div><![CDATA[<audio controls><source type="audio/mp3" src="foo.mp3"></audio>]]></div>"""

      val item = new ItemBuilder()
        .addToItemBody(audioInCData)
        .xml()

      val zip = new Builder()
        .addItem("1", item)
        .addMp3("foo.mp3", "/test.mp3")
        .build()

      val outPath = zip.getParent.resolve("out.zip")

      RunHelper.run(
        zip.toAbsolutePath.toString,
        outPath.toAbsolutePath.toString,
        "kds",
        None,
        """{"scoringType" : "SBAC"}"""
      )

      val playerDefJson = loadFirstPlayerDefJson(outPath).getOrElse(Json.obj())

      logger.info(d(playerDefJson))

      (playerDefJson \ "components" \ "foo" \ "componentType").as[String] must_== "corespring-audio"
      (playerDefJson \ "components" \ "foo" \ "fileName").as[String] must_== "foo.mp3"
      (playerDefJson \ "xhtml" ).as[String].contains("<corespring-audio id=\"foo\"></corespring-audio>") must_== true
    }
  }
}
