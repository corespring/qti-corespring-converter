package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.{ExtendedTextInteractionTransformer => CorespringExtendedTextInteractionTransformer, InteractionTransformer}
import play.api.libs.json.{Json, JsObject}

import scala.xml.Node

object ExtendedTextInteractionTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    CorespringExtendedTextInteractionTransformer.interactionJs(qti, manifest)
      .map{ case(id, json) => id -> json.deepMerge(Json.obj("feedback" -> Json.obj("feedbackType" -> "none"))) }

  override def transform(node: Node, manifest: Node): Seq[Node] =
    CorespringExtendedTextInteractionTransformer.transform(node, manifest)

}
