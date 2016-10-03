package org.corespring.conversion.qti.interactions

import play.api.libs.json.JsObject

import scala.xml.Node

/**
  * An InteractionTransformer which throws an exception for transformation of an unsupported interactiont type.
  */
class UnsupportedInteractionTransformer(interactionType: String) extends InteractionTransformer {

  override def transform(node: Node, manifest: Node): Seq[Node] = node.label match {
    case `interactionType` =>
      throw new IllegalArgumentException(s"Unsupported interaction type: $interactionType for ${(manifest \\ "@identifier")}")
    case _ => node
  }

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = Map.empty[String, JsObject]

}
