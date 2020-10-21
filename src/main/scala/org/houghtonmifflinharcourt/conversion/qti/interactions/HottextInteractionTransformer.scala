package org.houghtonmifflinharcourt.conversion.qti.interactions

import java.util.UUID.randomUUID

import org.corespring.conversion.qti.interactions.pie.InteractionTransformer
import play.api.libs.json._

import scala.xml._

object HottextInteractionTransformer extends InteractionTransformer {
  override def interactionJs(qti: Node, manifest: Node) = (qti)
    .map(implicit qtinode => {

      val componentId = (qti \ "@identifier").text.trim
      var markupString : String = ""
      var isItembodyPromptAdded = false;
      def correctResponses(node : Node): Seq[JsString] = {
        val values: Seq[Node] = (responseDeclaration(node, qti) \\ "value").toSeq
        values.map(n => JsString(n.text.trim))
      }
      def prompt(node: Node): String = {
        var itemBodyText ="";
        if(!isItembodyPromptAdded) {
          itemBodyText = (qti \\ "itemBody").map { n => removeNamespaces(n).child.filterNot(e => e.label == "hottextInteraction" || e.label == "rubricBlock").mkString.toString().trim }(0)
        }
        isItembodyPromptAdded = true;
        (node \ "prompt").length match {
          case length: Int if length == 1 => itemBodyText +  (removeNamespaces(node) \\ "prompt").map{ n => n.child.mkString.trim }(0)
          case _ => itemBodyText
        }
      }
      def maxSelections(node: Node): Int = {
        ((node \\ "@maxChoices").text) match {
          case text: String => (node \\ "@maxChoices").text.toInt
          case _ => 1
        }
      }
      def isCorrect(node : Node, parentNode: Node ): Boolean = {
        val choiceText = (node \ "@identifier").text;
        correctResponses(parentNode).indexOf(JsString(choiceText)) match {
          case -1 =>  false
          case _ =>  true
        }
      }
      def markup(): String = {
        (qti \\ "hottextInteraction").map{ n =>
          markupString += "<br/><select-text id=\"" + (n \ "@responseIdentifier").text + "\"></select-text>"
        }
        markupString
      }
      def text(node : Node): String = {
        var itemBodyText ="";
        itemBodyText = (node).map { n => removeNamespaces(n).child.filterNot(e => e.label == "feedbackInline" || e.label == "rubricBlock").mkString.toString().trim }(0)
        itemBodyText = itemBodyText.replace("$", "\\$");
        itemBodyText = """<hottext[^>]*>(?s)(.*?)<\/hottext>[;]*""".r.replaceAllIn(itemBodyText, m =>
          (m.group(1).toString()))
        itemBodyText
      }
      val json = Json.obj(
        "id" -> randomUUID,
        "name" -> (qti \ "@identifier").text,
        "collectionIds" -> Json.toJson(List("test").distinct),
        "config" -> Json.obj(
          "markup" ->   markup(),//JsString("<select-text id=\"" + (qti \ "@identifier").text + "\"></select-text>"),
          "models" ->  JsArray((qti \\ "hottextInteraction").map { n =>
            Json.obj(
              "id" -> (n \ "@responseIdentifier").text,
              "maxSelections" -> maxSelections(n),
              "partialScoring" -> false,
              "rationale" -> "Rationale goes here.",
              "prompt" -> prompt(n),
              "element" -> JsString("select-text"),
              "promptEnabled" -> true,
              "mode" -> "sentence",
              "text" -> text(n),
              "tokens" -> JsArray((n \\ "hottext").map { hc =>
                Json.obj(
                  "text" -> removeNamespaces(hc).child.filterNot(e => e.label == "feedbackInline").mkString.trim,
                  "start" -> JsNumber(text(n).indexOf(removeNamespaces(hc).child.filterNot(e => e.label == "feedbackInline").mkString.trim)),
                  "end" -> JsNumber(text(n).indexOf(removeNamespaces(hc).child.filterNot(e => e.label == "feedbackInline").mkString.trim) + removeNamespaces(hc).child.filterNot(e => e.label == "feedbackInline").mkString.trim.length),
                  "correct" -> JsBoolean(isCorrect(hc,n))
                )
              }
              )
             )
          }),
          "elements" -> Json.obj(
            "select-text" -> "@pie-element/select-text@latest"
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
