package org.corespring.conversion.qti

import com.keydatasys.conversion.qti.{KDSMode, MULTIPART}
import org.corespring.container.js.rhino.score.CustomScoreProcessor
import play.api.libs.json.{JsObject, JsValue}
import play.api.libs.json.Json.{obj, arr}


class TwoPointFlagSpec extends BaseRunner {

  //  sequential = true
  sequential
  /**
    * 664014 has the metadata/lom/general/parccTwoPointScoring flag - which means that we should normalize the score.
    */

  val correctSession = obj(
    "components" -> obj(
      "RESPONSE11" -> obj("answers" -> "-frac{sqrt5}3"),
      "RESPONSE2" -> obj("answers" -> arr("1", "4")),
      "RESPONSE31" -> obj("answers" -> "0.75"),
      "RESPONSE4" -> obj("answers" -> "4")
    )
  )
  val session = obj(
    "components" -> obj(
      "RESPONSE11" -> obj("answers" -> "foo"),
      "RESPONSE2" -> obj("answers" -> arr("1","4")),
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

//    "kds --sourceId 664014 SBAC - score is 1 if all are right" should {
//
//      "score correctly" in {
//        val converted = convert("664014", MULTIPART.id, KDSMode.SBAC, partsCount = 4, twoPointScoring = false)
//        val playerDefinition = getPlayerDef(converted).get
//
//        logger.info(s"scoring: \n${(playerDefinition \ "customScoring").as[String]}")
//        val result = CustomScoreProcessor.score(playerDefinition, correctSession.as[JsValue], outcomes.as[JsValue]).as[JsObject]
//        (result \ "summary" \ "score").as[Float] === 1
//      }
//    }
  //
  //  "kds --sourceId 664014 SBAC - score is 0 if any are wrong" should {
  //
  //    "score correctly" in {
  //      val converted = convert("664014", MULTIPART.id, KDSMode.SBAC, partsCount = 4, twoPointScoring = false)
  //      val playerDefinition = getPlayerDef(converted).get
  //      val result = CustomScoreProcessor.score(playerDefinition, session.as[JsValue], outcomes.as[JsValue]).as[JsObject]
  //      (result \ "summary" \ "score").as[Float] === 0
  //    }
  //  }
  //
  //    "kds --sourceId 664014 PARCC - score 0.75 for 3/4 right" should {
  //
  //      "score correctly" in {
  //        val converted = convert("664014", MULTIPART.id, KDSMode.PARCC, partsCount = 4, twoPointScoring = true)
  //        val playerDefinition = getPlayerDef(converted).get
  //        val result = CustomScoreProcessor.score(playerDefinition, session.as[JsValue], outcomes.as[JsValue]).as[JsObject]
  //        (result \ "summary" \ "score").as[Float] === 0.75
  //      }
  //    }

  "kds --sourceId 664014 PARCC - score 1.0 for 4/4 right" should {

    "score correctly" in {
      val converted = convert("664014", MULTIPART.id, KDSMode.PARCC, partsCount = 4, twoPointScoring = true)
      val playerDefinition = getPlayerDef(converted).get
      val result = CustomScoreProcessor.score(playerDefinition, correctSession.as[JsValue], outcomes.as[JsValue]).as[JsObject]
      (result \ "summary" \ "score").as[Float] === 1.0
    }
  }
}



