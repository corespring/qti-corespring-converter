package com.keydatasys.conversion.qti.util

import play.api.libs.json._

import scala.xml.SAXParseException

trait HtmlProcessor extends EntityEscaper {

  def preprocessHtml(html: String) = try {
    escapeEntities(Windows1252EntityTransformer.transform(html))
  } catch {
    case e: SAXParseException => {
      System.err.println(s"Err: ${e.getMessage}")
      html
    }
  }

  def postprocessHtml(html: String) = unescapeEntities(html)

  def postprocessHtml(jsValue: JsValue): JsValue = try {
    jsValue match {
      case jsObject: JsObject => JsObject(jsObject.fields.map{ case (key, value) => {
        (key, postprocessHtml(value))
      } })
      case jsArray: JsArray => JsArray(jsArray.value.map{ value => postprocessHtml(value) })
      case jsString: JsString => JsString(postprocessHtml(jsString.value.replaceAll("<br>", "<br/>")))
      case _ => jsValue
    }
  } catch {
    case e: SAXParseException => {
      System.err.println(s"Err2: ${e.getMessage}")
      jsValue
    }
  }

}
