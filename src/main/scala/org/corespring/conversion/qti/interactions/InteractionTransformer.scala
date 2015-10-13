package org.corespring.conversion.qti.interactions

import org.corespring.common.json.JsonUtil
import org.corespring.common.xml.XMLNamespaceClearer
import play.api.libs.json._

import scala.xml._
import scala.xml.transform.RewriteRule

/**
 * Base class for a transformation object which takes in QTI XML nodes and returns CoreSpring JSON+XML formats.
 */
abstract class InteractionTransformer extends RewriteRule with XMLNamespaceClearer with JsonUtil {

  def interactionJs(qti: Node, manifest: Node): Map[String, JsObject]
  def transform(node: Node, manifest: Node): Seq[Node]

  def transform(ns: Seq[Node], manifest: Node): Seq[Node] = {
    val changed = ns.flatMap(node => transform(node, manifest))
    if (changed.length != ns.length || (changed, ns).zipped.exists(_ != _)) changed
    else ns
  }

}


trait Transformer {

  def transform(qti: Node, manifest: Node): Node

}