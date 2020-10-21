package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.common.util.XHTMLCleaner
import java.util.UUID.randomUUID
import org.corespring.conversion.qti.interactions.pie.InteractionTransformer
import play.api.libs.json.{JsArray, JsBoolean, JsString, Json}
import scala.xml._

object InlineChoiceInteractionTransformer extends InteractionTransformer with XHTMLCleaner {

  override def interactionJs(qti: Node, manifest: Node) = (qti)
    .map(implicit qtinode => {

      val componentId = (qti \ "@identifier").text.trim
      var icIndex : Int = -1

      def correctResponses(node : Node): Seq[JsString] = {
        val values: Seq[Node] = (responseDeclaration(node, qti) \\ "value").toSeq
        values.map(n => JsString(n.text.trim))
      }
      def prompt(node: Node): String = {
        (node \ "prompt").length match {
          case length: Int if length == 1 => (removeNamespaces(node) \\ "prompt").map{ n => n.child.mkString.trim }(0)
          case _ => ""
        }
      }
      def isCorrect(node : Node, parentNode: Node ): Boolean = {
        val choiceText = (node \ "@identifier").text;
        correctResponses(parentNode).indexOf(JsString(choiceText)) match {
          case -1 =>  false
          case _ =>  true
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
        itemBodyText = """<inlineChoiceInteraction[^>]*>(?s)(.*?)<\/inlineChoiceInteraction>[;]*""".r.replaceAllIn(itemBodyText,  m => "{{"+getIndex()+"}}")
        icIndex = -1;
        itemBodyText
      }

     def getChoices() = {
       var choices : Map[String,JsArray] = Map.empty

       for (w <- 0 until (qti \\ "inlineChoiceInteraction").size) {
         choices = choices + (w.toString() -> JsArray(((qti \\ "inlineChoiceInteraction")(w) \\ "inlineChoice").map { n =>
           Json.obj(
             "label" -> removeNamespaces(n).child.filterNot(e => e.label == "feedbackInline").mkString.trim,
             "value" -> (n \\ "@identifier").text,
             "correct" -> JsBoolean(isCorrect(n,(qti \\ "inlineChoiceInteraction")(w))),
             "feedback" -> Json.obj(
               "type" -> "none",
               "value" -> ((n \ "feedbackInline").length match {
                 case length: Int if length == 1 => (removeNamespaces(n) \\ "feedbackInline").map{ n => n.child.toString() }(0)
                 case _ => ""
               })
             )
           )
         }))
       }
       choices
     }
      val json = Json.obj(
        "id" -> randomUUID,
        "name" -> (qti \ "@identifier").text,
        "collectionIds" -> Json.toJson(List("test").distinct),
        "config" -> Json.obj(
          "markup" ->   JsString("<inline-dropdown id=\"" + (qti \ "@identifier").text + "\"></inline-dropdown>"),
          "models" -> JsArray((qti).map { n =>
            Json.obj(
              "id" -> (qti \ "@identifier").text,
              "prompt" -> prompt(qti),
              "element" -> JsString("inline-dropdown"),
              "markup" -> "",
              "disabled" -> false,
              "mode" -> "gather",
              "promptEnabled" -> true,
              "shuffle" -> true,
              "markup" -> markup(),
              "choices" ->  getChoices()
          )}),
          "elements" -> Json.obj(
            "inline-dropdown" -> "@pie-element/inline-dropdown@latest"
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
