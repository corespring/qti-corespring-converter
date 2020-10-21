package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.pie.InteractionTransformer
import java.util.UUID.randomUUID
import play.api.libs.json._
import scala.xml._

object OrderInteractionTransformer extends InteractionTransformer {

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
        itemBodyText = (qti \\ "itemBody").map { n => removeNamespaces(n).child.filterNot(e => e.label == "orderInteraction" || e.label == "rubricBlock").mkString.toString().trim }(0)
      }
      isItembodyPromptAdded = true;
      val orderInteractionText =  node.map { n =>  removeNamespaces(n).child.filterNot(e => e.label == "simpleChoice").mkString.trim}
      (node \ "prompt").length match {
        case length: Int if length == 1 => itemBodyText +  (removeNamespaces(node) \\ "prompt").map{ n => n.child.mkString.trim }(0)
        case _ => itemBodyText + orderInteractionText(0).toString()
      }
    }

    def markup(): String = {
      ((qti \\ "orderInteraction")).map{ n =>
        (n.label match {
          case "orderInteraction" =>  markupString += "<br/><pie-placement-ordering id=\"" + (n \ "@responseIdentifier").text + "\"></pie-placement-ordering>"
        })
      }
      markupString
    }
    val json = Json.obj(
      "id" -> randomUUID,
      "name" -> (qti \ "@identifier").text,
      "collectionIds" -> Json.toJson(List("test").distinct),
      "config" -> Json.obj(
      "markup" ->   markup,
      "models" -> JsArray(((qti \\ "orderInteraction")).map { ci =>

        Json.obj(
        "id" -> (ci \ "@responseIdentifier").text,
        "prompt" -> prompt(ci),
          "element" -> "pie-placement-ordering",
        "keyMode" -> JsString("letters"),
        "choices" -> JsArray(((ci \\ "simpleChoice").toSeq).map { n =>
          Json.obj(
            "label" -> removeNamespaces(n).child.filterNot(e => e.label == "feedbackInline").mkString.trim,
            "id" -> (n \\ "@identifier").text,
            "feedback" -> Json.obj(
                            "type" -> "none",
                            "value" -> ((n \ "feedbackInline").length match {
                              case length: Int if length == 1 => (removeNamespaces(n) \\ "feedbackInline").map{ n => n.child.toString() }(0)
                              case _ => ""
                            })
                          )
          )
        }),
          "correctResponse" -> correctResponses(ci))
      }),
      "elements" -> Json.obj(
        "pie-placement-ordering" -> "@pie-element/placement-ordering@latest"
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
