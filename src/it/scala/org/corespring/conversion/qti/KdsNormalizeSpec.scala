package org.corespring.conversion.qti

import com.keydatasys.conversion.qti.KDSMode
import play.api.libs.json.Json.{obj, prettyPrint, arr}
import play.api.libs.json.{JsObject, JsValue}

import org.corespring.container.js.rhino.score.CustomScoreProcessor
class KdsNormalizeSpec extends BaseRunner {


  override def sourceId = "664255"

  override def scoringType = KDSMode.PARCC

  "kds --sourceId 664255" should {
    "score correctly" in {
      println(playerDef)


      val playerDefinition = playerDef.map(json(zip, _)).get

      (playerDefinition \ "customScoring")

      val session = obj(
        "components" -> obj(
          "RESPONSE1" -> obj("answers" -> arr(1, 4)),
          "RESPONSE21" -> obj("answers" -> "12000")
        )
      )

      val outcomes = obj(
        "RESPONSE1" -> obj("correctness" -> "correct"),
        "RESPONSE21" -> obj("correctness" -> "correct")
      )

      val result = CustomScoreProcessor.score(playerDefinition, session.as[JsValue], outcomes.as[JsValue]).as[JsObject]
      (result \ "summary" \ "score").as[Float] === 1.0
    }
  }
}
