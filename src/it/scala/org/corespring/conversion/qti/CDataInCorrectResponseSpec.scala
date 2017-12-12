package org.corespring.conversion.qti

import java.nio.file.Files

import org.slf4j.LoggerFactory
import org.specs2.mutable.Specification
import play.api.libs.json.Json

class CDataInCorrectResponseSpec
  extends Specification with BaseRunnerUtils {

  private val logger = LoggerFactory.getLogger(this.getClass)

  /**
    * An example of this would be kds:sbac: 665204.xml
    */
  val rd = """<responseDeclaration identifier="RESPONSE21" cardinality="single" baseType="string">
    <correctResponse>
      <value><![CDATA[-8<-2]]></value>
      <value><![CDATA[-2>-8]]></value>
    </correctResponse>
  </responseDeclaration>"""

  val body = <textEntryInteraction responseIdentifier="RESPONSE21" expectedLength="10"/>

  val item = new ItemBuilder()
    .addToItemBody(body)
    .addResponseDeclaration(rd)
    .xml()

  val manifestZip = new Builder().addItem("1", item).build()

  "build item" should {

    "work" in {

      val out = Files.createTempDirectory(s"${this.getClass.getSimpleName}")
      val outPath = out.resolve("out.zip")
      RunHelper.run(
        manifestZip.toAbsolutePath.toString,
        outPath.toAbsolutePath.toString,
        "kds",
        None,
      """{"scoringType": "SBAC"}""" )

      val playerDefJson = loadFirstPlayerDefJson(outPath).getOrElse(Json.obj())

      logger.info(s"playerDefJson: ${Json.prettyPrint(playerDefJson)}")
      (playerDefJson \ "components" \ "RESPONSE21" \ "correctResponses" \ "values").as[Seq[String]] must_== Seq("-8<-2", "-2>-8")
    }
  }
}
