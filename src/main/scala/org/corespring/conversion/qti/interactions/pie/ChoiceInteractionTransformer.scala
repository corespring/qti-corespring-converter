package org.corespring.conversion.qti.interactions.pie

import java.util.UUID.randomUUID
import play.api.libs.json._

import scala.xml._

object ChoiceInteractionTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node) = ((qti \\ "choiceInteraction") ++ (qti \\ "inlineChoiceInteraction"))
    .map(implicit node => {

    val componentId = (node \ "@responseIdentifier").text.trim
    var choiceIndex : Int=  0;

    def correctResponses: Seq[JsString] = {
      val values: Seq[Node] = (responseDeclaration(node, qti) \\ "value").toSeq
      values.map(n => JsString(n.text.trim))
    }
    def choiceType(node: Node): String = {
      correctResponses.length match {
        case length: Int if length > 1 => "checkbox"
        case _ => if ((node \\ "@maxChoices").text == "1") "radio" else "checkbox"
      }
    }
    def prompt(node: Node): String = {
      (node \ "prompt").length match {
        case length: Int if length == 1 => node.child.filter(e => e.label == "prompt").text.trim
        case _ => ""
      }
    }
    def isCorrect(node : Node ): Boolean = {
      val choiceText = (node \ "@identifier").text;
      correctResponses.indexOf(JsString(choiceText)) match {
        case -1 =>  false
        case _ =>  true
      }
    }
    def increment(): String = {
      choiceIndex += 1;
      (choiceIndex+64).toChar.toString
    }
    val json = Json.obj(
      "id" -> randomUUID,
      "name" -> (qti \ "@identifier").text,
      "collectionIds" -> Json.toJson(List("test").distinct),
      "config" -> Json.obj(
        "markup" ->   (node.label match {
          case "choiceInteraction" => JsString("<br/><multiple-choice pie-id=\""+(qti \ "@identifier").text+"\"></multiple-choice>")
          case "inlineChoiceInteraction" => JsString("<br/><inline-choice pie-id=\""+(qti \ "@identifier").text+"\"></inline-choice>")
        }),
        "models" -> JsArray(node.map { n =>
          Json.obj(
          "id" -> (qti \ "@identifier").text,
          "prompt" -> prompt(node),
          "element" -> (node.label match {
            case "choiceInteraction" => JsString("multiple-choice")
            case "inlineChoiceInteraction" => JsString("inline-choice")
          }),
          "choiceMode" -> JsString(choiceType(qti)),
          "keyMode" -> JsString("letters"),
          "choices" -> JsArray(((node \\ "simpleChoice").toSeq ++ (node \\ "inlineChoice")).map { n =>
            Json.obj(
              "label" -> n.child.filterNot(e => e.label == "feedbackInline").mkString.trim,
              "value" -> increment,
              "correct" -> JsBoolean(isCorrect(n)),
              "feedback" -> Json.obj(
                              "type" -> (n \ "feedbackInline").text,
                              "value" -> (n \ "@identifier").text
                            )
            )
          }))}),
        "element" -> Json.obj(
          (node.label match {
            case "choiceInteraction" => "multiple-choice" -> "@pie-element/multiple-choice@2.1.0"
            case "inlineChoiceInteraction" => "inline-choice" -> "@pie-element/inline-choice@2.1.0"
          })
        )
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
