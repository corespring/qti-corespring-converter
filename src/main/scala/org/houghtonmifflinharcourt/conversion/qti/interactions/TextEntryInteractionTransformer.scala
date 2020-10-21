package org.houghtonmifflinharcourt.conversion.qti.interactions

import java.util.UUID.randomUUID

import org.corespring.common.util.XHTMLCleaner
import org.corespring.conversion.qti.interactions.pie.InteractionTransformer
import play.api.libs.json.{JsArray, JsBoolean, JsString, Json}

import scala.xml._

object TextEntryInteractionTransformer extends InteractionTransformer with XHTMLCleaner {

  override def interactionJs(qti: Node, manifest: Node) = (qti)
    .map(implicit qtinode => {

      val componentId = (qti \ "@identifier").text.trim
      var icIndex : Int = -1

      def prompt(node: Node): String = {
        (node \ "prompt").length match {
          case length: Int if length == 1 => (removeNamespaces(node) \\ "prompt").map{ n => n.child.mkString.trim }(0)
          case _ => ""
        }
      }

      def getIndex(): Int = {
        icIndex += 1;
        icIndex
      }

      def markup(): String = {
       var itemBodyText ="";
        icIndex = -1;
       itemBodyText = (qti \\ "itemBody").map { n => removeNamespaces(n).child.filterNot(e => e.label == "rubricBlock").mkString.toString().trim }(0)
        itemBodyText = """<textEntryInteraction*(?s)(.*?)\/>[;]*""".r.replaceAllIn(itemBodyText,  m => "{{"+getIndex()+"}}")
        icIndex = -1;
        itemBodyText
      }
      val json = Json.obj(
        "id" -> randomUUID,
        "name" -> (qti \ "@identifier").text,
        "collectionIds" -> Json.toJson(List("test").distinct),
        "config" -> Json.obj(
          "markup" ->   JsString("<pie-explicit-constructed-response id=\"" + (qti \ "@identifier").text + "\"></pie-explicit-constructed-response>"),
          "models" -> JsArray((qti).map { n =>
            Json.obj(
              "id" -> (qti \ "@identifier").text,
              "prompt" -> prompt(qti),
              "element" -> JsString("explicit-constructed-response"),
              "markup" -> "",
              "disabled" -> false,
              "mode" -> "gather",
              "promptEnabled" -> true,
              "shuffle" -> true,
              "markup" -> markup()
          )}),
          "elements" -> Json.obj(
            "pie-explicit-constructed-response" -> "@pie-element/explicit-constructed-response@latest"
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
