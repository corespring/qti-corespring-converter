package org.corespring.conversion.qti


class KdsRunnerSpec extends BaseRunner {

  override def sourceId = "670508"

  "kds --sourceId 670508" should {


    val profileJson = profile.map(json(zip, _)).get

    "add scoringType to profile.json" in {
      (profileJson \ "taskInfo" \ "extended" \ "kds" \ "scoringType").as[String] must_== "SBAC"
    }

    "add sourceId to profile.json" in {
      (profileJson \ "taskInfo" \ "extended" \ "kds" \ "sourceId").as[String] must_== "670508"
    }

    "add title to profile.json" in {
      (profileJson \ "taskInfo" \ "title").as[String] must_== "670508 - SBAC"
    }

    "add description to profile.json" in {
      (profileJson \ "taskInfo" \ "description").as[String] must_== "670508 - SBAC"
    }

    "add corespring-number-line as the componentType for RESPONSE1" in {
      playerDef.map(json(zip, _)).map { json =>
        (json \ "components" \ "RESPONSE1" \ "componentType").as[String] must_== "corespring-number-line"
      }.getOrElse(ko)
    }

    "add the inline css" in {
      playerDef.map(json(zip, _)).map { json =>
        println(json \ "xhtml")
        (json \ "xhtml").as[String].contains(".qti.kds") must_== true
      }.getOrElse(ko)

    }
  }
}


