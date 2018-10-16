package org.corespring.conversion.qti


import com.keydatasys.conversion.qti.KDSMode
import play.api.libs.json.Json.{obj, prettyPrint, arr}
import play.api.libs.json.{JsObject, JsValue}

import org.corespring.container.js.rhino.score.CustomScoreProcessor
class KdsNonNormalizeSpec extends BaseRunner {


  override def sourceId = "660295"

  override def scoringType = KDSMode.PARCC

  "kds --sourceId 660295" should {
    "score correctly" in {
      println(playerDef)


      val playerDefinition = playerDef.map(json(zip, _)).get

      (playerDefinition \ "customScoring")

      val session = obj(
        "components" -> obj(
          "RESPONSE1" -> obj("answers" -> "1") ,
          "RESPONSE2" -> obj("answers" -> "3")
        )
      )

      val outcomes = obj(
        "RESPONSE1" -> obj("correctness" -> "correct"),
        "RESPONSE2" -> obj("correctness" -> "correct")
      )

      val result = CustomScoreProcessor.score(playerDefinition, session.as[JsValue], outcomes.as[JsValue]).as[JsObject]
      (result \ "summary" \ "score").as[Float] === 1.0
    }
  }
}

