package com.scalapeno.rhinos

import org.specs2.mutable._
import play.api.libs.json._

class RhinosRuntimeSpec extends SpecificationWithJUnit    {
  
  "RhinosRuntime.eval[T](...)" should {
    var rhinos: RhinosRuntime = null
    
    step {
      rhinos = new RhinosRuntime()
    }

    "return None when the script is empty" in {
      val result = rhinos.eval("""""")

      result must beNone
    }

    "throw an exception when the script is not valid" in {
      rhinos.eval("""var x = bla bla bla""") must throwA[Exception]
    }

    "return None when the script does not return a result" in {
      val result = rhinos.eval("""var x = 42;""")

      result must beNone
    }

    "return None when the script returns a Javascript null" in {
      val result = rhinos.eval("""var b = null; b;""")

      result must beNone
    }

    "return Some(true) when the script returns a Javascript true" in {
      val result = rhinos.eval("""var b = true; b;""")
      result match {
        case Some(jsBoolean: play.api.libs.json.JsBoolean) if jsBoolean.value == true => success
        case Some(value) => failure("was not true")
        case _ => failure("No result!")
      }
    }

    "return Some(false) when the script returns a Javascript false" in {
      val result = rhinos.eval("""var b = false; b;""")
      result match {
        case Some(jsBoolean: play.api.libs.json.JsBoolean) if jsBoolean.value == false => success
        case Some(value) => failure("was not false")
        case _ => failure("No result!")
      }
    }

    "return Some[JsNumber] when the script returns a Javascript number" in {
      val result = rhinos.eval("""var n = 3; n;""")
      result match {
        case Some(jsNumber: play.api.libs.json.JsNumber) if jsNumber.value == 3 => success
        case Some(value) => failure("was not 3")
        case _ => failure("No result!")
      }
    }

    "return Some[JsNumber] when the script returns a Javascript number" in {
      val result = rhinos.eval("""var n = 3141521424; n;""")
      result match {
        case Some(jsNumber: play.api.libs.json.JsNumber) if jsNumber.value == 3141521424L => success
        case Some(value) => failure("was not 3141521424")
        case _ => failure("No result!")
      }
    }

    "return Some[JsNumber] and the script returns a Javascript number" in {
      val result = rhinos.eval("""var n = 3.1415; n;""")
      result match {
        case Some(jsNumber: play.api.libs.json.JsNumber) if jsNumber.value == BigDecimal(3.1415) => success
        case Some(value) => failure("was not 3.1415")
        case _ => failure("No result!")
      }
    }

    "return Some[JsNumber] when the script returns a Javascript number" in {
      val result = rhinos.eval("""var n = 1313133.141586193338; n;""")
      result match {
        case Some(jsNumber: play.api.libs.json.JsNumber) if jsNumber.value == BigDecimal(1313133.141586193338) => success
        case Some(value) => failure("was not 1313133.141586193338")
        case _ => failure("No result!")
      }
    }


    "return Some[JsString] when the script returns a character" in {
      val result = rhinos.eval("""var c = 'c'; c;""")
      result match {
        case Some(jsString: play.api.libs.json.JsString) if jsString.value == "c" => success
        case Some(value) => failure("""was not "c"""")
        case _ => failure("No result!")
      }
    }

    "return Some[String] when the script returns a Javascript string" in {
      val result = rhinos.eval("""var s = "some string!"; s;""")
      result match {
        case Some(jsString: play.api.libs.json.JsString) if jsString.value == "some string!" => success
        case Some(value) => failure("""was not "some string!"""")
        case _ => failure("No result!")
      }
    }

  }
  
}
