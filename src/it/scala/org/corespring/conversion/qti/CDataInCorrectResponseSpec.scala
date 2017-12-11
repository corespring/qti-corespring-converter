package org.corespring.conversion.qti

import java.io.File
import java.nio.file.Files
import java.util.zip.ZipFile

import scala.collection.JavaConversions._
import org.slf4j.LoggerFactory
import org.specs2.mutable.Specification
import play.api.libs.json.Json

class CDataInCorrectResponseSpec extends Specification with BaseRunnerUtils {

  private val logger = LoggerFactory.getLogger(this.getClass)
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

      val zip = new ZipFile(new File(outPath.toAbsolutePath.toString))

      val playerDef = zip.entries.find {
        e =>
          logger.info(s"e.getName: ${e.getName}")
          e.getName.contains("player-definition.json")
      }

      val playerDefJson = playerDef.map{ e =>
        json(zip, e)
      }

      (item \\ "correctResponse" \ "value").head.text must_== "-8<-2"
    }
  }
}
