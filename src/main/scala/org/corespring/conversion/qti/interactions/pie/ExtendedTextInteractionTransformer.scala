package org.corespring.conversion.qti.interactions.pie

import play.api.libs.json._
import java.util.UUID.randomUUID
import scala.xml._

object ExtendedTextInteractionTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    (qti \\ "extendedTextInteraction").map(implicit node => {
      val componentId = (node \ "@responseIdentifier").text.trim

      def prompt(node: Node): String = {
        val itemBodyText =  (qti \\ "itemBody").map { n =>  removeNamespaces(n).child.filterNot(e => e.label == "extendedTextInteraction" || e.label == "rubricBlock").mkString.toString().trim}
        val extendedTextInteractionText =  node.map { n =>  removeNamespaces(n).child.mkString.trim}
        (node \ "prompt").length match {
          case length: Int if length == 1 => itemBodyText(0).toString() +  (removeNamespaces(node) \\ "prompt").map{ n => n.child.mkString.trim }(0)
          case _ => itemBodyText(0) + extendedTextInteractionText(0)
        }
      }
      val json = Json.obj(
        "id" -> randomUUID,
        "name" -> (node \ "@responseIdentifier").text,
        "collectionIds" -> Json.toJson(List("test").distinct),
        "markup" ->   JsString("<br/><extended-text-entry id='"+(node \ "@responseIdentifier").text+"'></extended-text-entry>"),
        "models" -> JsArray(node.map{ n=>
          Json.obj(
            "id" -> (n \ "@responseIdentifier").text,
            "prompt" -> prompt(n),
            "element" -> "extended-text-entry",
            "width" -> "300px",
            "height" -> "300px",
            "expectedLength" -> optForAttr[JsNumber]("expectedLength"),
            "expectedLines" -> optForAttr[JsNumber]("expectedLines"),
            "maxStrings" -> optForAttr[JsNumber]("maxStrings"),
            "minStrings" -> optForAttr[JsNumber]("minStrings")
            )}),
        "elements" -> Json.obj(
          "extended-text-entry" -> "@pie-element/extended-text-entry@latest"
        ),
        "searchMetaData" -> Json.obj(
          "internal_author" -> "",
          "internal_status" -> "",
          "hmh_standard" -> JsArray(),
          "hmh_locale" -> "en_US",
          "hmh_cognitiveDemand" -> "",
          "hmh_depthOfKnowledge" -> ""
        )
      )
      componentId -> json
    }).toMap
}
