package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json.JsObject

import scala.xml._

object RubricBlockTransformer extends InteractionTransformer with ImageConverter {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = Map.empty

  override def transform(node: Node, manifest: Node) = node match {
    case node: Node if ("rubricBlock" == node.label) => node.attribute("use") match {
      case Some(attribute) => attribute.text match {
        case "stimulus" => convertObjectsToImages(<div class="stimulus">{node.child}</div>)
        case _ => Seq.empty[Node]
      }
      case _ => Seq.empty[Node]
    }
    case node: Node if ("sampleBlock" == node.label) => Seq.empty[Node]
    case _ => node
  }
}
