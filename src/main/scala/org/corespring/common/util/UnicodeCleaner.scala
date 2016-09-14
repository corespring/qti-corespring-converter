package org.corespring.common.util

import play.api.libs.json._

/**
 * Progress Testing content contains some unusual unicode characters. This utility trait removes them from markup.
 */
trait UnicodeCleaner {

  def cleanUnicode(string: String): String = string.replaceAll("[\u0000-\u001f]", "").replaceAll("\\p{C}", "")

  def cleanUnicode(json: JsValue): JsValue = json match {
    case jsObject: JsObject => JsObject(jsObject.fields.map{ case (key, value) => (key, cleanUnicode(value)) })
    case jsArray: JsArray => JsArray(jsArray.value.map{ value => cleanUnicode(value) })
    case jsString: JsString => JsString(cleanUnicode(jsString.value))
    case _ => json
  }

}
