package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.pie.InteractionTransformer
import play.api.libs.json._
import java.util.UUID.randomUUID
import scala.xml._

object MatchInteractionTransformer extends InteractionTransformer {
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
          itemBodyText = (qti \\ "itemBody").map { n => removeNamespaces(n).child.filterNot(e => e.label == "matchInteraction" || e.label == "rubricBlock").mkString.toString().trim }(0)
        }
        isItembodyPromptAdded = true;
        val choiceInteractionText =  node.map { n =>  removeNamespaces(n).child.filterNot(e => e.label == "simpleMatchSet").mkString.trim}
        (node \ "prompt").length match {
          case length: Int if length == 1 => itemBodyText +  (removeNamespaces(node) \\ "prompt").map{ n => n.child.mkString.trim }(0)
          case _ => itemBodyText + choiceInteractionText(0).toString()
        }
      }
      def markup(): String = {
        ((qti \\ "matchInteraction")).map{ n =>
          (n.label match {
            case "matchInteraction" =>  markupString += "<br/><pie-match id=\"" + (n \ "@responseIdentifier").text + "\"></pie-match>"
          })
        }
        markupString
      }
      val json = Json.obj(
        "id" -> randomUUID,
        "name" -> (qti \ "@identifier").text,
        "config" -> Json.obj(
          "markup" ->   markup(),
          "models" -> JsArray((qti \\ "matchInteraction").map { mi =>

            Json.obj(
              "id" -> (mi \ "@responseIdentifier").text,
              "prompt" -> prompt(mi),
              "element" -> JsString("pie-match"),
              "enableImages" -> true,
              "choiceMode" -> JsString(choiceType(mi)),
              "headers" -> JsArray(((mi \ "cornerText").text.toString match {
                case empty if (empty.isEmpty) => JsString("")
                case nonEmpty: String => JsString((mi \ "cornerText").text.toString)
                })
                +: ((mi \\ "simpleMatchSet").tail \\ "simpleAssociableChoice").map {
                n => JsString(removeNamespaces(n).child.filterNot(e => e.label == "feedbackInline").mkString.trim)
              }),
              "layout" -> JsNumber(((mi \\ "simpleMatchSet").tail \\ "simpleAssociableChoice").size + 1),
              "lockChoiceOrder" -> true,
              "partialScoring" -> false,
              "rows" -> JsArray(((mi \\ "simpleMatchSet").head \\ "simpleAssociableChoice").map{ n =>
                Json.obj(
                  "id" ->  (n \ "@identifier").text,
                  "title" -> removeNamespaces(n).child.filterNot(e => e.label == "feedbackInline").mkString.trim,
                  "values" -> JsArray(((mi \\ "simpleMatchSet")(1) \\ "simpleAssociableChoice").map(n=>JsBoolean(false)))
                )
              })
            )
          }),
          "elements" -> Json.obj(
            "pie-match" -> "@pie-element/match@latest"
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
