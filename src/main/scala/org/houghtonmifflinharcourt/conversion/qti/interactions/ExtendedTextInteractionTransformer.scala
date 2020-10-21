package org.houghtonmifflinharcourt.conversion.qti.interactions
import org.corespring.conversion.qti.interactions.pie.{InteractionTransformer}
import java.util.UUID.randomUUID
import play.api.libs.json.{JsArray, JsNumber, JsObject, Json}

import scala.xml._

object ExtendedTextInteractionTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    (qti).map(implicit node => {
      val componentId = (node \ "@identifier").text.trim
      var markupString : String = ""
      var isItembodyPromptAdded = false;

      def prompt(node: Node): String = {
        var itemBodyText ="";
        if(!isItembodyPromptAdded) {
          itemBodyText = (qti \\ "itemBody").map { n =>  removeNamespaces(n).child.filterNot(e => e.label == "extendedTextInteraction" || e.label == "rubricBlock").mkString.toString().trim}(0)
        }
        isItembodyPromptAdded = true;
        val extendedTextInteractionText =  node.map { n =>  removeNamespaces(n).child.mkString.trim}
        (node \ "prompt").length match {
          case length: Int if length == 1 => itemBodyText +  (removeNamespaces(node) \\ "prompt").map{ n => n.child.mkString.trim }(0)
          case _ => itemBodyText + extendedTextInteractionText(0)
        }
      }
      def markup(): String = {
        (qti \\ "extendedTextInteraction").map{ n =>
          markupString += "<br/><extended-text-entry id=\"" + (n \ "@responseIdentifier").text + "\"></extended-text-entry>"
        }
        markupString
      }
      val json = Json.obj(
        "id" -> randomUUID,
        "name" -> (node \ "@identifier").text,
        "collectionIds" -> Json.toJson(List("test").distinct),
        "config" -> Json.obj(
          "markup" ->   markup(), //JsString("<br/><extended-text-entry id='"+(node \ "@responseIdentifier").text+"'></extended-text-entry>"),
          "models" -> JsArray((qti \\ "extendedTextInteraction").map{ eti=>
            Json.obj(
              "id" -> (eti \ "@responseIdentifier").text,
              "prompt" -> prompt(eti),
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
          )),
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
