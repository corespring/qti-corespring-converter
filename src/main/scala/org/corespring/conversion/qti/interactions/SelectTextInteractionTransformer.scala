package org.corespring.conversion.qti.interactions

import org.jsoup.Jsoup
import play.api.libs.json._

import scala.xml._

import scala.collection.JavaConversions._

object SelectTextInteractionTransformer extends InteractionTransformer {

  object Defaults {
    val shuffle = false
  }

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    (qti \\ "hottextInteraction").map(implicit node => {
      (node \ "@responseIdentifier").text -> Json.obj(
        "componentType" -> "corespring-select-text",
        "model" -> Json.obj(
          "config" -> Json.obj(
            "selectionUnit" -> "custom",
            "maxSelections" -> ((node \ "@maxChoices").text.trim match {
              case "" => 0
              case _ => (node \ "@maxChoices").text.trim.toInt
            }),
            "label" -> "",
            "availability" -> "all",
            "passage" -> {
              val doc = Jsoup.parse(node.child.mkString)
              doc.getElementsByTag("hottext").foreach(hottext => {
                val csToken = doc.createElement("span")
                csToken.addClass("cs-token")
                csToken.html(hottext.html)
                hottext.replaceWith(csToken)
              })
              doc.select("body").html
            }
          )
        ),
        "allowPartialScoring" -> false,
        "correctResponse" -> Json.obj(
          "value" -> {
            val correctIds = (responseDeclaration(node, qti) \ "correctResponse" \\ "value").map(_.text.trim)
            val doc = Jsoup.parse(node.child.mkString)
            doc.getElementsByTag("hottext").zipWithIndex.filter{ case (element, index) => {
              correctIds.contains(element.attr("identifier"))
            }}.map{ case (element, index) => index }
          }
        ),
        "feedback" -> Json.obj(
          "correctFeedbackType" -> "default",
          "partialFeedbackType" -> "default",
          "incorrectFeedbackType" -> "default"
        ),
        "partialScoring" -> Json.arr(Json.obj())
      )
    }).toMap

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case elem: Elem if elem.label == "selectTextInteraction" => {
      val identifier = (elem \ "@responseIdentifier").text
      <corespring-select-text id={ identifier }></corespring-select-text>
    }
    case _ => node
  }



}
