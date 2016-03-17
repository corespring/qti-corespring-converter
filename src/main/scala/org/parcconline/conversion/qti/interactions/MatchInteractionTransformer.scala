package org.parcconline.conversion.qti.interactions

import com.keydatasys.conversion.qti.util.EntityEscaper
import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json._

import scala.xml.Node

object MatchInteractionTransformer extends InteractionTransformer with EntityEscaper {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    (qti \\ "matchInteraction").map(implicit node => {
      (node \ "@responseIdentifier").text -> Json.obj(
        "componentType" -> "corespring-dnd-categorize",
        "correctResponse" -> JsObject(
          (qti \\ "responseDeclaration").find(rd => (rd \ "@identifier").text == (node \ "@responseIdentifier").text)
            .map(rd => (rd \ "correctResponse" \ "value").map(value => {
              value.text.split(" ") match {
                case Array(key, value) => key -> Json.arr(JsString(value))
                case _ => throw new Exception("Wat")
              }
            })).getOrElse(Seq.empty)
        ),
        "feedback" -> Json.obj(
          "correctFeedbackType" -> "none",
          "partialFeedbackType" -> "none",
          "incorrectFeedbackType" -> "none"
        ),
        "allowPartialScoring" -> false,
        "model" -> Json.obj(
          "categories" -> ((node \ "simpleMatchSet").last \ "simpleAssociableChoice").map{ choice => Json.obj(
            "id" -> (choice \ "@identifier").text,
            "label" -> encodeSafeEntities(choice.child.mkString)
          )},
          "choices" -> ((node \ "simpleMatchSet").head \ "simpleAssociableChoice").map{ choice => Json.obj(
            "id" -> (choice \ "@identifier").text,
            "label" -> encodeSafeEntities(choice.child.mkString),
            "moveOnDrag" -> true
          )},
          "config" -> Json.obj(
            "shuffle" -> false,
            "answerAreaPosition" -> "above",
            "categoriesPerRow" -> 1,
            "choicesPerRow" -> 2,
            "choicesLabel" -> "",
            "removeAllAfterPlacing" -> true
          )
        )
      )
    }).toMap

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case node: Node if (node.label == "matchInteraction") =>
      <corespring-dnd-categorize id={ (node \\ "@responseIdentifier").text }></corespring-dnd-categorize>
    case _ => node
  }

}
