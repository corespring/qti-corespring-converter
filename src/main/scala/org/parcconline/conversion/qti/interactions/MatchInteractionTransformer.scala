package org.parcconline.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json.JsObject

import scala.xml.Node

class MatchInteractionTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = ???

  override def transform(node: Node, manifest: Node): Seq[Node] = ???

}
