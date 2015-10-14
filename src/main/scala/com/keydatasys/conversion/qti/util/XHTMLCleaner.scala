package com.keydatasys.conversion.qti.util

import scala.xml._
import scala.xml.transform._

trait XHTMLCleaner {

  val labelMap = Map("partBlock" -> "div", "partBody" -> "div", "selectedResponseParts" -> "div")

  implicit class NodeHelper(node: Node) {

    def convertNonXHTMLElements: Option[Node] = node.isEmpty match {
      case false => new RuleTransformer(new RewriteRule {
        override def transform(node: Node) = (node, labelMap.get(node.label)) match {
          case (e: Elem, Some(newLabel)) => e.copy(label = newLabel, attributes = Null)
          case _ => node
        }
      }).transform(node).headOption
      case _ => Some(node)
    }

  }


  implicit class StringHelper(string: String) {

    def cleanWhitespace: String = {
      val r = "\\n(.*)\\n".r
      string.trim match {
        case r(noBreaks) => noBreaks.trim
        case string => string
      }
    }

  }


}
