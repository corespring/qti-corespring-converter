package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.{HottextInteractionTransformer => CoreSpringHottextInteractionTransformer, InteractionTransformer}
import play.api.libs.json.{Json, JsObject}

import scala.xml.Node

object HottextInteractionTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    CoreSpringHottextInteractionTransformer.interactionJs(qti, manifest).map{ case (id, json) =>
      id -> json.deepMerge(
        Json.obj("feedback" -> Json.obj(
          "correctFeedbackType" -> "none",
          "partialFeedbackType" -> "none",
          "incorrectFeedbackType" -> "none"
        ))
      )
    }

  override def transform(node: Node, manifest: Node): Seq[Node] =
    CoreSpringHottextInteractionTransformer.transform(node, manifest)

}
