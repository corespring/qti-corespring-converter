package com.keydatasys.conversion.qti.processing

import java.io.InputStream

import org.apache.commons.io.IOUtils
import org.corespring.container.js.rhino.score.CustomScoreProcessor
import org.slf4j.LoggerFactory
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import play.api.libs.json.{JsObject, JsValue}
import play.api.libs.json.Json._

import scala.xml.{Node, XML}

case class TestData(qti: Node, item: JsObject, session: JsObject)

class ProcessingTransformerTest extends Specification {

  case class score(val outcomes: JsObject) extends Scope {

    def process(js: String, item: JsObject, session: JsObject, outcomes: JsObject): JsObject = {
      val i = item ++ obj("customScoring" -> js)
      CustomScoreProcessor.score(i.as[JsValue], session.as[JsValue], outcomes.as[JsValue]).as[JsObject]
    }

    val TestData(qti, item, session) = loadTestData("one")
    val responseProcessing = transformer.toJs(qti)
    val denominator = responseProcessing.map(t => t.responseVars.length)

    val js = responseProcessing.map(rp => V2JavascriptWrapper.wrap(rp, denominator)).get
    logger.info(s"js: \n$js\n")
    val result = process(js, item, session, outcomes)
  }

  val logger = LoggerFactory.getLogger(this.getClass)

  val transformer = new ProcessingTransformer {

  }

  def mkOutcomes(key: String = "score", value: Double = 1) = {
    obj(
      "RESPONSE116" -> obj(key -> value),
      "RESPONSE114" -> obj(key -> value),
      "RESPONSE117" -> obj(key -> value),
      "RESPONSE115" -> obj(key -> value),
      "RESPONSE113" -> obj(key -> value)
    )
  }

  def loadTestData(name: String): TestData = {

    def stream(file: String) = this.getClass.getResourceAsStream(s"/custom-scoring/$name/$file")

    def json(is: InputStream): JsObject = parse(IOUtils.toString(is)).as[JsObject]

    val is = this.getClass().getResourceAsStream(s"/custom-scoring/$name/qti.xml")
    TestData(
      XML.load(is),
      json(stream("item.json")),
      json(stream("session.json"))
    )
  }

  val topScore = obj(
    "score" -> 1.0,
    "points" -> 5,
    "maxPoints" -> 5,
    "percentage" -> "100",
    "score113" -> 1,
    "score114" -> 1,
    "score115" -> 1,
    "score116" -> 1,
    "score117" -> 1,
    "__corespringInternal" -> obj(
      "divider" -> 5,
      "responseCount" -> 5
    )
  )

  val halfScore = obj(
    "score" -> 0.5,
    "points" -> 2.5,
    "maxPoints" -> 5,
    "percentage" -> "50",
    "score113" -> 0.5,
    "score114" -> 0.5,
    "score115" -> 0.5,
    "score116" -> 0.5,
    "score117" -> 0.5,
    "__corespringInternal" -> obj(
      "divider" -> 5,
      "responseCount" -> 5
    )
  )

  "V2JavascriptWrapperTest" should {
    "return summary object" in new score(mkOutcomes()) {
      (result \ "summary").as[JsObject] must_== topScore
    }

    "return summary when outcomes are 0.5" in new score(mkOutcomes(value = 0.5)) {
      (result \ "summary").as[JsObject] must_== halfScore
    }

    "return summary when legacyScore is 1.0" in new score(mkOutcomes(key = "legacyScore")) {
      (result \ "summary").as[JsObject] must_== topScore
    }


    "return summary when correctNum is 1.0" in new score(mkOutcomes(key = "correctNum")) {
      (result \ "summary").as[JsObject] must_== topScore
    }

    "return summary when correctNum and score is  0.5" in new score(mkOutcomes(key = "correctNum", value = 0.5)) {
      (result \ "summary").as[JsObject] must_== halfScore
    }

  }
}
