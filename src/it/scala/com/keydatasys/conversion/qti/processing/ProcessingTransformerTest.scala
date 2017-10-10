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

case class TestData(qti:Node, item: JsObject, session: JsObject)

class ProcessingTransformerTest extends Specification {

  case class score(val outcomes:JsObject) extends Scope{

    def process(js:String, item:JsObject, session: JsObject, outcomes: JsObject) : JsObject = {
      val i  = item ++ obj("customScoring" -> js)
      CustomScoreProcessor.score(i.as[JsValue], session.as[JsValue], outcomes.as[JsValue]).as[JsObject]
    }

    val TestData(qti, item, session) = loadTestData("one")
    val responseProcessing = transformer.toJs(qti)
    val js = responseProcessing.map( rp => transformer.wrap(rp) ).get
    logger.info(s"js: \n$js\n")
    val result = process(js, item, session, outcomes)
  }

  val logger = LoggerFactory.getLogger(this.getClass)

  val transformer = new ProcessingTransformer {

  }

  def mkOutcomes(key:String = "score", value: Double = 1) = {
    obj(
      "RESPONSE116" -> obj(key -> value),
      "RESPONSE114" -> obj(key -> value),
      "RESPONSE117" -> obj(key -> value),
      "RESPONSE115" -> obj(key -> value),
      "RESPONSE113" -> obj(key -> value)
    )
  }

  def loadTestData(name:String) : TestData = {

    def stream(file:String) = this.getClass.getResourceAsStream(s"/custom-scoring/$name/$file")

    def json(is:InputStream): JsObject = parse(IOUtils.toString(is)).as[JsObject]

    val is = this.getClass().getResourceAsStream(s"/custom-scoring/$name/qti.xml")
    TestData(
      XML.load(is),
      json(stream("item.json")),
      json(stream("session.json"))
    )
  }

  "V2JavascriptWrapperTest" should {
    "return 1.0 for score key" in new score(mkOutcomes()) {
      (result \ "summary" \ "score").as[Float] must_== 1
    }

    "return 0.5 for score key when score is 0.5" in new score(mkOutcomes(value = 0.5)) {
      (result \ "summary" \ "score").as[Float] must_== 0.5
    }

    "return 1.0 for score when key is legacyScore is 1.0" in new score(mkOutcomes(key="legacyScore")) {
      (result \ "summary" \ "score").as[Float] must_== 1.0
    }

    "return 0.5 for score when key is legacyScore and score is 0.5" in new score(mkOutcomes(key="legacyScore", value = 0.5)) {
      (result \ "summary" \ "score").as[Float] must_== 0.5
    }

    "return 1.0 for score when key is correctNum is 1.0" in new score(mkOutcomes(key="correctNum")) {
      (result \ "summary" \ "score").as[Float] must_== 1.0
    }

    "return 0.5 for score when key is correctNum and score is  0.5" in new score(mkOutcomes(key="correctNum", value = 0.5)) {
      (result \ "summary" \ "score").as[Float] must_== 0.5
    }

  }
}
