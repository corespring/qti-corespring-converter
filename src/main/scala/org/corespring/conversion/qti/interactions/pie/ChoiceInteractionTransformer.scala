package org.corespring.conversion.qti.interactions.pie

import java.util.UUID.randomUUID

import play.api.libs.json._

import scala.xml._
import scala.xml.transform.{RewriteRule, RuleTransformer}

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
        case _ => "radio"//if ((node \\ "@maxChoices").text == "1") "radio" else "checkbox"
      }
    }
    def prompt(node: Node): String = {
      val itemBodyText =  (qti \\ "itemBody").map { n =>  removeNamespaces(n).child.filterNot(e => e.label == "choiceInteraction" || e.label == "rubricBlock").mkString.toString().trim}
      val choiceInteractionText =  node.map { n =>  removeNamespaces(n).child.filterNot(e => e.label == "simpleChoice").mkString.trim}
      (node \ "prompt").length match {
        case length: Int if length == 1 => itemBodyText(0).toString() +  (removeNamespaces(node) \\ "prompt").map{ n => n.child.mkString.trim }(0)
        case _ => itemBodyText(0).toString() + choiceInteractionText(0).toString()
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
      "markup" ->   (node.label match {
        case "choiceInteraction" => JsString("<br/><multiple-choice id=\""+(qti \ "@identifier").text+"\"></multiple-choice>")
        case "inlineChoiceInteraction" => JsString("<br/><inline-choice id=\""+(qti \ "@identifier").text+"\"></inline-choice>")
      }),
      "models" -> JsArray(node.map { n =>
        Json.obj(
        "id" -> (qti \ "@identifier").text,
        "prompt" -> prompt(n),
        "element" -> (n.label match {
          case "choiceInteraction" => JsString("multiple-choice")
          case "inlineChoiceInteraction" => JsString("inline-choice")
        }),
        "choiceMode" -> JsString(choiceType(qti)),
        "keyMode" -> JsString("letters"),
        "choices" -> JsArray(((n \\ "simpleChoice").toSeq ++ (n \\ "inlineChoice")).map { n =>
          Json.obj(
            "label" -> removeNamespaces(n).child.filterNot(e => e.label == "feedbackInline").mkString.trim,
            "value" -> increment,
            "correct" -> JsBoolean(isCorrect(n)),
            "feedback" -> Json.obj(
                            "type" -> "none",
                            "value" -> ((n \ "feedbackInline").length match {
                              case length: Int if length == 1 => (removeNamespaces(n) \\ "feedbackInline").map{ n => n.child.toString() }(0)
                              case _ => ""
                            })
                          )
          )
        }))}),
      "elements" -> Json.obj(
        (node.label match {
          case "choiceInteraction" => "multiple-choice" -> "@pie-element/multiple-choice@2.7.3"
          case "inlineChoiceInteraction" => "inline-choice" -> "@pie-element/inline-choice@2.7.3"
        })
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
