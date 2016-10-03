package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json.{Json, JsObject}

import scala.xml.Node

class DefaultFeedbackTransformer(transformer: InteractionTransformer) extends InteractionTransformer {

  implicit class DefaultFeedbackJson(jsObject: JsObject) {

    def defaultFeedback = jsObject.deepMerge(
      Json.obj("feedback" -> Json.obj(
        "correctFeedbackType" -> "none",
        "partialFeedbackType" -> "none",
        "incorrectFeedbackType" -> "none"
      ))
    )

  }

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    transformer.interactionJs(qti, manifest).map{ case (id, json) => id -> json.defaultFeedback }

  override def transform(node: Node, manifest: Node): Seq[Node] = transformer.transform(node, manifest)

}

object DefaultFeedbackTransformer {
  def apply(transformer: InteractionTransformer) = new DefaultFeedbackTransformer(transformer)
}
