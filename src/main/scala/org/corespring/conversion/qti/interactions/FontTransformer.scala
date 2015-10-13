package org.corespring.conversion.qti.interactions

import play.api.libs.json._

import scala.xml._

/**
 * Removes style="font-size:2pt;" if it exists on a node.
 */
object FontTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = Map.empty

  override def transform(node: Node, manifest: Node) = {
    node match {
      case elem: Elem if (elem \ "@style").text.contains("font-size:2pt") =>
        elem.copy(attributes = elem.attributes.remove("style"))
      case _ => node
    }
  }

}
