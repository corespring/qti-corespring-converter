package org.corespring.conversion.qti

import play.api.libs.json._

import scalaz.Validation

abstract class AbstractItemExtractor {

  val ids: Seq[String]
  val metadata: Map[String, Validation[Error, Option[JsValue]]]
  val itemJson: Map[String, Validation[Error, JsValue]]

}

object AbstractItemExtractor {
  object Errors {
    val cannotCreateItem = "There was an error saving the item to the database"
    def fileMissing(filename: String) = s"Provided item source did not include $filename"
    def jsonParseError(filename: String) = s"$filename did not contain valid json"
    def metadataParseError(field: String) = s"There was an error parsing $field from metadata"
  }
}
