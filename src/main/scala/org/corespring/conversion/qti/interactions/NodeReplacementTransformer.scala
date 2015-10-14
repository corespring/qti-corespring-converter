package org.corespring.conversion.qti.interactions


import scala.xml._

import play.api.libs.json._

abstract class NodeReplacementTransformer extends InteractionTransformer {

  def labelToReplace: String
  def replacementNode: Elem

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case elem: Elem if elem.label == labelToReplace => replacementNode.copy(child = elem.child.map(clearNamespace))
    case _ => node
  }

  override def interactionJs(qti: Node, manifest: Node) = Map.empty[String, JsObject]

}
