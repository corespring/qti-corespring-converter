package com.scalapeno.rhinos

import play.api.libs.json._
import org.specs2.mutable.Specification

class JavascriptProcessorSpec extends Specification with JavascriptProcessor {

  "js" should {

    "not allow arbitrary code execution" in {
      js("java.lang.System.println('foo')") must throwAn[EcmaErrorWithSource]
    }

    "kill thread after 10 seconds" in {
      js(
        """
          var x = "";
          while (true) {
            x += "0";
          }
        """) must throwA[ThreadDeath]
    }

    "return String values correctly" in {
      val string = "Yay!"
      js(s""" "$string";""") match {
        case Some((script:String,jsString: JsString)) if jsString.value === string => success
        case _ => failure
      }
    }

    "with variables" in {

      "puts String in scope" in {
        val stringValue = "great!"
        js("str;", Map("str" -> stringValue)) match {
          case Some((script:String,jsString: JsString)) => {
            jsString.value === stringValue
            success
          }
          case _ => failure("Fail!")
        }
      }

      "puts JsString in scope" in {
        val stringValue = "great!"
        js("str;", Map("str" -> JsString(stringValue))) match {
          case Some((script:String,jsString: JsString)) => {
            jsString.value === stringValue
            success
          }
          case _ => failure("Fail!")
        }
      }

      def parseDouble(numberLike: Any) =
        try {
          numberLike match {
            case jsNumber: JsNumber => jsNumber.value.toDouble
            case _ => numberLike.toString.toDouble
          }
        } catch {
          case _ => throw new IllegalArgumentException(s"Error parsing double from $numberLike")
        }

      def testNumber(number: Any) = {
        js("number;", Map("number" -> number)) match {
          case Some((script:String,jsNumber: JsNumber)) => jsNumber.value.toDouble === parseDouble(number); success
          case _ => failure(s"Failed for value $number")
        }
      }

      "puts numbers in scope" in {
        testNumber(3)
        testNumber(3L)
        testNumber(3.14159)
      }

      "puts JsNumbers in scope" in {
        testNumber(JsNumber(3))
        testNumber(JsNumber(3L))
        testNumber(JsNumber(3.14159))
      }

      "puts JsObjects in scope" in {
        val jsObject = Json.obj(
          "val1" -> "one",
          "val2" -> 2
        )

        js("object;", Map("object" -> jsObject)) match {
          case Some(jsObject: JsObject) => {
            (jsObject \ "val1").asInstanceOf[JsString].value === "one"
            (jsObject \ "val2").asInstanceOf[JsNumber].value === 2
            success
          }
          case Some(thingElse) => success
          case _ => failure(s"Failed for value $jsObject")
        }
      }

      "puts JsArrays in scope" in {
        val array = Json.arr(1, 2, 3)
        js("array;", Map("array" -> array)) match {
          case Some((script:String,jsArray: JsArray)) => success
          case _ => failure
        }
      }

    }
  }

  "validIdentifier" should {

    "not validate reserved words" in {
      reservedWords.find(validIdentifier(_)).isEmpty === true
    }

    "not validate identifiers starting with numbers" in {
      validIdentifier("2chains") === false
    }

    "validate valid identifiers" in {
      Seq("a", "b", "myVar", "greatVarName").find(!validIdentifier(_)).isEmpty === true
    }

  }

  "toVar" should {

    "translate strings" in {
      toVar("a", "test") === """var a = "test";"""
    }

    "translate integers" in {
      toVar("a", 3) === "var a = 3;"
    }

    "translate arrays" in {
      val array = Json.arr("a", 4)
      toVar("test", array) === """var test = ["a",4];"""
    }

    "translate maps" in {
      val map = Json.obj(
        "users" -> Json.arr(
          Json.obj(
            "name" -> "bob",
            "age" -> 31,
            "email" -> "bob@gmail.com"
          ),
          Json.obj(
            "name" -> "kiki",
            "age" -> 25,
            "email" -> JsNull
          )
        )
      )
      toVar("test", map) === """var test = {"users":[{"name":"bob","age":31,"email":"bob@gmail.com"},{"name":"kiki","age":25,"email":null}]};"""
    }

  }


}