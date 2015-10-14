package com.keydatasys.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.{TextEntryInteractionTransformer => CorespringTextEntryInteractionTransformer, Transformer, InteractionTransformer}
import org.corespring.conversion.qti.transformers.InteractionRuleTransformer
import play.api.libs.json._

import scala.xml._

class TextEntryInteractionTransformer(qti: Node) extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    CorespringTextEntryInteractionTransformer(qti).interactionJs(qti, manifest).map {
      case (id, json) => id -> json.deepMerge(Json.obj(
        "feedback" -> Json.obj(
          "correctFeedbackType" -> "default",
          "incorrectFeedbackType" -> "default"),
        "correctResponses" -> Json.obj(
          "feedback" -> Json.obj(
            "type" -> "default",
            "value" -> "Correct!")),
        "incorrectResponses" -> Json.obj(
          "feedback" -> Json.obj(
            "type" -> "default",
            "value" -> "Good try, but the correct answer is <random selection from correct answers>."))))
    }.toMap

  override def transform(node: Node, manifest: Node): Seq[Node] =
    CorespringTextEntryInteractionTransformer(qti).transform(node, manifest)

}

object TextEntryInteractionTransformer extends Transformer {

  def apply(qti: Node) = new TextEntryInteractionTransformer(qti)

  def transform(qti: Node, manifest: Node): Node =
    new InteractionRuleTransformer(new CorespringTextEntryInteractionTransformer(qti)).transform(qti, manifest).head

}
