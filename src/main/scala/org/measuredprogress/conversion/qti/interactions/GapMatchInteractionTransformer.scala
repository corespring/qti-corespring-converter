package org.measuredprogress.conversion.qti.interactions

import org.corespring.common.html.JsoupParser
import org.corespring.conversion.qti.interactions.InteractionTransformer
import org.jsoup.Jsoup
import play.api.libs.json._
import play.twirl.api.TemplateMagic.javaCollectionToScala

import scala.xml.{Elem, Node}

object GapMatchInteractionTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = (qti \\ "gapMatchInteraction").map(implicit node => {
    val identifier = (node \ "@responseIdentifier").text
    identifier -> Json.obj(
      "componentType" -> "corespring-drag-and-drop-inline",
      "correctResponse" -> (responseDeclaration(node, qti) \\ "correctResponse" \\ "value").map(_.text.split(" ") match {
        case Array(choiceId, answerAreaId) => answerAreaId -> Seq(choiceId)
        case _ => throw new IllegalStateException("Did not have a one-to-one relation for responses")
      }).toMap[String, Seq[String]],
      "model" -> Json.obj(
        "answerAreaXhtml" -> {
          val doc = JsoupParser.parse(node.child.mkString)
          doc.getElementsByTag("prompt").foreach(prompt => {
            val div = doc.createElement("div")
            div.attr("class", "prompt")
            div.html(prompt.html)
            prompt.replaceWith(div)
          })
          doc.getElementsByTag("gap").foreach(gap => {
            val answerArea = doc.createElement("answer-area-inline-csdndi")
            answerArea.attr("id", gap.attr("identifier"))
            gap.replaceWith(answerArea)
          })
          doc.select("gaptext").remove()
          doc.select("body").html
        },
        "answerAreas" -> JsoupParser.parse(node.child.mkString).select("gap").map(gap => Json.obj("id" -> gap.attr("identifier"))),
        "choices" -> JsoupParser.parse(node.child.mkString).select("gaptext").map(gaptext =>
          Json.obj("label" -> gaptext.html, "labelType" -> "text", "id" -> gaptext.attr("identifier"))
        ),
        "config" -> Json.obj(
          "shuffle" -> false,
          "choiceAreaLabel" -> "",
          "choiceAreaLayout" -> "horizontal",
          "choiceAreaPosition" -> "below"
        )
      )
    )
  }).toMap

  override def transform(node: Node, manifest: Node): Seq[Node] = {
    val identifier = (node \ "@responseIdentifier").text
    node match {
      case elem: Elem if elem.label == "gapMatchInteraction" =>
        <corespring-drag-and-drop-inline id={ identifier }></corespring-drag-and-drop-inline>
      case _ => node
    }
  }

}
