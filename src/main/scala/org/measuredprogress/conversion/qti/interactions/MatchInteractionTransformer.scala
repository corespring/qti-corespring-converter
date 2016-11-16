package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json._

import scala.xml.Node

object MatchInteractionTransformer extends InteractionTransformer with ImageConverter {

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case node: Node if (node.label == "matchInteraction") =>
      <p class="prompt">{ (node \ "prompt").map(_.child).flatten }</p> ++
          <corespring-match id={ (node \\ "@responseIdentifier").text }/>
    case _ => node
  }

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = (qti \\ "matchInteraction").map(implicit node => {

    (node \ "@responseIdentifier").text -> Json.obj(
      "componentType" -> "corespring-match",
      "correctResponse" -> correctResponse(qti),
      "allowPartialScoring" -> false,
      "partialScoring" -> Json.arr(Json.obj()),
      "feedback" -> Json.obj(
        "correctFeedbackType" -> "none",
        "partialFeedbackType" -> "none",
        "incorrectFeedbackType" -> "none"
      ),
      "model" -> Json.obj(
        "columns" -> {
          val cols: Seq[JsObject] = ((node \\ "simpleMatchSet").tail \\ "simpleAssociableChoice").map(col => Json.obj("labelHtml" -> convertObjectsToImages(col.child).toString))
          Json.obj("labelHtml" -> JsString((node \ "prompt").headOption.map(n => convertObjectsToImages(n.child)).getOrElse(Seq.empty).toString)) +: cols
        },
        "rows" -> ((node \\ "simpleMatchSet").head \\ "simpleAssociableChoice").map(row => Json.obj(
          "id" -> (row \ "@identifier").text,
          "labelHtml" -> convertObjectsToImages(row.child).toString
        )),
        "config" -> Json.obj(
          "inputType" -> inputType(qti),
          "shuffle" -> false
        )
      )
    )
  }).toMap

  private def correctResponse(qti: Node)(implicit node: Node) = {
    def matchSet(id: String, answers: Seq[String]): Seq[Boolean] =
      ((qti \\ "simpleMatchSet").last \\ "simpleAssociableChoice").map(choice => answers.contains((choice \ "@identifier").text))

    (((node \\ "simpleMatchSet").head \\ "simpleAssociableChoice").zipWithIndex.map{ case (choice, index) => {
      Json.obj(
        "id" -> (choice \ "@identifier").text,
        "matchSet" -> matchSet(
          (choice \ "@identifier").text,
          (responseDeclaration(node, qti) \\ "correctResponse" \\ "value")
            .filter(_.text.trim.startsWith((choice \ "@identifier").text))
            .map(_.text.split(" ").drop(1).toSeq).flatten)
      )
    }})
  }

  private def inputType(qti: Node)(implicit node: Node) = {
    println("WAT WAT WAT WAT")
    val rows = (responseDeclaration(node, qti) \\ "correctResponse" \\ "value").map(_.text.split(" ").headOption).flatten
    (rows.distinct.size != rows.size) match {
      case true => "checkbox"
      case _ => "radiobutton"
    }
  }


}
