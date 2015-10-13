package org.corespring.common.json

import org.specs2.mutable.Specification
import play.api.libs.json._

class JsonUtilSpec extends Specification with JsonUtil {

  "optForAttr" should {

    "parse Some[JsString] from String attribute" in {
      val attribute = "class"
      val value = "great"
      implicit val node = <div class={value}></div>
      optForAttr[JsString](attribute) must beEqualTo(Some(JsString(value)))
    }

    "parse Some[JsNumber] from number attribute" in {
      val attribute = "count"
      val value = 2
      implicit val node = <div count={value.toString}></div>
      optForAttr[JsNumber](attribute) must beEqualTo(Some(JsNumber(value)))
    }

    "pasre Some[JsBoolean] from boolean attribute" in {
      val attribute = "awesome"
      val value = true
      implicit val node = <div awesome={value.toString}></div>
      optForAttr[JsBoolean](attribute) must beEqualTo(Some(JsBoolean(value)))
    }

    "parse None from missing attribute" in {
      implicit val node = <div/>
      optForAttr[JsString]("id") must be equalTo(None)
    }

  }

  "partialObj" should {

    val stringAttribute = "string"
    val stringValue = "present"
    val noneAttribute = "none"

    val result = partialObj(
      stringAttribute -> Some(JsString(stringValue)),
      noneAttribute -> None
    )

    "include Some[JsString] in result JSON" in {
      (result \ stringAttribute).as[String] must beEqualTo(stringValue)
    }

    "not include None in result JSON" in {
      (result \ noneAttribute).asOpt[JsValue] must beEmpty
    }

  }

}
