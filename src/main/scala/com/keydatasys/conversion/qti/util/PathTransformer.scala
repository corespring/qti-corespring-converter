package com.keydatasys.conversion.qti.util

import scala.xml._
import scala.xml.transform._

/**
 * KDS image and video resources are prefixed by ./ instead of / in markup, so we need to rewrite these.
 */
object PathTransformer {

  import PathFlattener._

  def transform(node: Node): Elem = {
    new RuleTransformer(new RewriteRule {
      override def transform(node: Node) = node match {
        case elem: Elem if (Seq("img", "source").contains(node.label) && (node \ "@src").text.toString != "") => rewriteSrc(elem)
        case _ => node
      }
    }).transform(node).head.asInstanceOf[Elem]
  }

  private def rewriteSrc(elem: Elem) = {
    elem % Attribute(null, "src", """\.\/(.*)""".r.replaceAllIn((elem \ "@src").text.toString, "$1").flattenPath, Null)
  }
}
