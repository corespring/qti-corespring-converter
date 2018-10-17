package org.corespring.conversion.qti

import com.keydatasys.conversion.qti.{KDSMode, MULTIPART}
import org.corespring.container.js.rhino.score.CustomScoreProcessor
import play.api.libs.json.{JsObject, JsValue}
import play.api.libs.json.Json.{obj}



class TwoPointFlagSpec extends NewKdsBaseRunner {


  /**
    * 664014 has the metadata/lom/general/parccTwoPointScoring flag - which means that we should normalize the score.
    */

  "kds --sourceId 664014 PARCC - twoPoint" should {

    "score correctly" in {

      val converted = convert("664014", MULTIPART.id, KDSMode.PARCC, partsCount = 4, twoPointScoring = true)


      val playerDefinition = getPlayerDef(converted).get //playerDef.map(json(zip, _)).get

      (playerDefinition \ "customScoring")

      val session = obj(
        "components" -> obj(
          "RESPONSE11" -> obj("answers" -> "foo"),
          "RESPONSE2" -> obj("answers" -> "1"),
          "RESPONSE31" -> obj("answers" -> "0.75"),
          "RESPONSE4" -> obj("answers" -> "4")
        )
      )

      val outcomes = obj(
        "RESPONSE11" -> obj("correctness" -> "correct"),
        "RESPONSE2" -> obj("correctness" -> "correct"),
        "RESPONSE31" -> obj("correctness" -> "correct"),
        "RESPONSE4" -> obj("correctness" -> "correct")
      )

      val result = CustomScoreProcessor.score(playerDefinition, session.as[JsValue], outcomes.as[JsValue]).as[JsObject]
      (result \ "summary" \ "score").as[Float] === 0.75
    }
  }

}



