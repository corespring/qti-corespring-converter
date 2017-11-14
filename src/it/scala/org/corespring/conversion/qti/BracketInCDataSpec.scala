package org.corespring.conversion.qti

import play.api.libs.json.JsObject
import play.api.libs.json.Json._

/**
  *
  * When we strip the CData:
  * <correctResponse>
  *            <value><![CDATA[-8<-2]]></value>
  *      </correctResponse>
  *
  * we get <value>-8<-2</value> - which is not valid xml
  *
  * This test asserts that this < gets escaped when xml parsing, and unescaped when written out.
  */
class BracketInCDataSpec extends BaseRunner {

  override def sourceId = "665204"

  "kds --sourceId 662504" should {

    val playerDefJson = playerDef.map(json(zip, _)).getOrElse {
      throw new RuntimeException("Not defined")
    }

    "it has the correct response" in {
      val components = (playerDefJson \ "components" \ "RESPONSE21").as[JsObject]
      logger.info(prettyPrint(components))
      (components \ "correctResponses" \ "values").as[Seq[String]] must_== Seq("-8<-2", "-2>-8")
    }

    "it has correct customScoring" in {
      val customScoring = (playerDefJson \ "customScoring").as[String]
      logger.info(customScoring)
      val js = """RESPONSE21 === "-8<-2" """.trim
      val jsTwo = """RESPONSE21 === "-2>-8" """.trim
      customScoring.contains(js) must_== true
      customScoring.contains(jsTwo) must_== true
    }

    "it has a fixed audio tag" in {
      (playerDefJson \ "xhtml").as[String].contains("""<corespring-audio id="foo"></corespring-audio>""") must_== true
    }
  }
}
