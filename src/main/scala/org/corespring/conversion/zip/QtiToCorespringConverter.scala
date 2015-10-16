package org.corespring.conversion.zip

import java.util.zip.ZipFile

import play.api.libs.json.JsObject

/**
 * Represents an interface which can translate a QTI zip file into a CoreSpring JSON zip file
 */
trait QtiToCorespringConverter {

  def convert(zip: ZipFile, path: String, metadata: Option[JsObject]): ZipFile

}
