package org.corespring.conversion.qti



import com.keydatasys.conversion.qti.KDSMode
import org.corespring.container.js.rhino.score.CustomScoreProcessor
import play.api.libs.json.{JsObject, JsValue}
import play.api.libs.json.Json.{obj, prettyPrint}

class EbsrQtiSpec extends BaseRunner {

  override def sourceId = "661656"

  override def scoringType = KDSMode.PARCC

  /**
    * 661656 has the metadata/lom/general/itemTypeId=11 flag -
    * which means that it is an ebsr item and should normalize the score.
    */

  s"kds --sourceId $sourceId" should {

    "work" in {

      println(playerDef)

      val playerDefinition = playerDef.map(json(zip, _)).get

      (playerDefinition \ "customScoring")

      val session = obj(
        "components" -> obj(
          "RESPONSE1" -> obj("answers" -> "4"),
          "RESPONSE2" -> obj("answers" -> "1")
        )
      )

      val outcomes = obj(
        "RESPONSE1" -> obj("correctness" -> "correct"),
        "RESPONSE2" -> obj("correctness" -> "correct")
      )

      val result = CustomScoreProcessor.score(playerDefinition, session.as[JsValue], outcomes.as[JsValue]).as[JsObject]
      (result \ "summary" \ "score").as[Float] ===1.0
    }
  }

}




