package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.common.util.XHTMLCleaner
import org.corespring.conversion.qti.interactions.pie.InteractionTransformer
import play.api.libs.json._
import java.util.UUID.randomUUID
import scala.xml._

object ChoiceInteractionTransformer extends InteractionTransformer with XHTMLCleaner {

  override def interactionJs(qti: Node, manifest: Node) = (qti)
    .map(implicit qtinode => {

      val componentId = (qti \ "@identifier").text.trim
      var choiceIndex : Int=  0
      var markupString : String = ""
      var isItembodyPromptAdded = false;


      def correctResponses(node : Node): Seq[JsString] = {
        val values: Seq[Node] = (responseDeclaration(node, qti) \\ "value").toSeq
        values.map(n => JsString(n.text.trim))
      }
      def choiceType(node: Node): String = {
        correctResponses(node).length match {
          case length: Int if length > 1 => "checkbox"
          case _ => "radio"
        }
      }
      def prompt(node: Node): String = {
        var itemBodyText ="";
        if(!isItembodyPromptAdded) {
          itemBodyText = (qti \\ "itemBody").map { n => removeNamespaces(n).child.filterNot(e => e.label == "choiceInteraction" || e.label == "rubricBlock").mkString.toString().trim }(0)
        }
        isItembodyPromptAdded = true;
        val choiceInteractionText =  node.map { n =>  removeNamespaces(n).child.filterNot(e => e.label == "simpleChoice").mkString.trim}
        (node \ "prompt").length match {
          case length: Int if length == 1 => itemBodyText +  (removeNamespaces(node) \\ "prompt").map{ n => n.child.mkString.trim }(0)
          case _ => itemBodyText + choiceInteractionText(0).toString()
        }
      }
      def isCorrect(node : Node, parentNode: Node ): Boolean = {
        val choiceText = (node \ "@identifier").text;
        correctResponses(parentNode).indexOf(JsString(choiceText)) match {
          case -1 =>  false
          case _ =>  true
        }
      }
      def increment(): String = {
        choiceIndex += 1;
        (choiceIndex+64).toChar.toString
      }

      def markup(): String = {
        ((qti \\ "choiceInteraction") ++ (qti \\ "inlineChoiceInteraction")).map{ n =>
          (n.label match {
            case "choiceInteraction" =>  markupString += "<br/><multiple-choice id=\"" + (n \ "@responseIdentifier").text + "\"></multiple-choice>"
            case "inlineChoiceInteraction" =>  markupString += "<br/><multiple-choice id=\"" + (n \ "@responseIdentifier").text + "\"></multiple-choice>"
          })
        }
        markupString
      }
      val json = Json.obj(
        "id" -> randomUUID,
        "name" -> (qti \ "@identifier").text,
        "collectionIds" -> Json.toJson(List("test").distinct),
        "config" -> Json.obj(
          "markup" ->   markup(),
          "models" -> JsArray(((qti \\ "choiceInteraction") ++ (qti \\ "inlineChoiceInteraction")).map { ci =>

            Json.obj(
              "id" -> (ci \ "@responseIdentifier").text,
              "prompt" -> prompt(ci),
              "element" -> (ci.label match {
                case "choiceInteraction" => JsString("multiple-choice")
                case "inlineChoiceInteraction" => JsString("inline-choice")
              }),
              "choiceMode" -> JsString(choiceType(ci)),
              "keyMode" -> JsString("letters"),
              "choices" -> JsArray(((ci \\ "simpleChoice").toSeq ++ (ci \\ "inlineChoice")).map { n =>
                Json.obj(
                  "label" -> removeNamespaces(n).child.filterNot(e => e.label == "feedbackInline").mkString.trim,
                  "value" -> increment,
                  "correct" -> JsBoolean(isCorrect(n,ci)),
                  "feedback" -> Json.obj(
                    "type" -> "none",
                    "value" -> ((n \ "feedbackInline").length match {
                      case length: Int if length == 1 => (removeNamespaces(n) \\ "feedbackInline").map{ n => n.child.toString() }(0)
                      case _ => ""
                    })
                  )
                )
              }))
          }),
          "elements" -> Json.obj(
            "multiple-choice" -> "@pie-element/multiple-choice@2.7.3"
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
