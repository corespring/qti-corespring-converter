package org.corespring.conversion.qti

import com.keydatasys.conversion.qti.KDSMode
import org.corespring.container.js.rhino.score.CustomScoreProcessor
import play.api.libs.json.{JsObject, JsValue}
import play.api.libs.json.Json.{obj, arr, prettyPrint}

class TwoPointUsePartsSpec extends BaseRunner {

  override def sourceId = "665182"

  override def scoringType = KDSMode.PARCC


  "kds --sourceId 665182" should {

    "score correctly" in {

      println(playerDef)

      val playerDefinition = playerDef.map(json(zip, _)).get

      (playerDefinition \ "customScoring")

      val session = obj(
        "components" -> obj(
          "RESPONSE11" -> obj("answers" -> "330"),
          "RESPONSE12" -> obj("answers" -> "880"),
          "RESPONSE13" -> obj("answers" -> "20"),
          "RESPONSE21" -> obj("answers" -> "18.50"),
          "RESPONSE3" -> obj("answers" -> arr(
            obj("id" -> "D", "left" -> 193, "top" -> 2),
            obj("id" -> "A", "left" -> 193, "top" -> 55),
            obj("id" -> "C", "left" -> 193, "top" -> 109),
            obj("id" -> "B", "left" -> 193, "top" -> 158)
          )),
          "RESPONSE41" -> obj("answers" -> "d")
        )
      )

      val outcomes = obj(
        "RESPONSE11" -> obj("correctness" -> "correct"),
        "RESPONSE12" -> obj("correctness" -> "correct"),
        "RESPONSE13" -> obj("correctness" -> "correct"),
        "RESPONSE21" -> obj("correctness" -> "correct"),
        "RESPONSE3" -> obj("correctness" -> "correct", "score" -> 1),
        "RESPONSE41" -> obj("correctness" -> "correct")
      )

      val result = CustomScoreProcessor.score(playerDefinition, session.as[JsValue], outcomes.as[JsValue]).as[JsObject]
      (result \ "summary" \ "score").as[Float] === 0.75
      (result \ "summary" \ "__corespringInternal").as[JsObject] === obj(
        "divider" -> 4,
        "responseCount" -> 6
      )
    }
  }

}




