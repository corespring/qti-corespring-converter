package org.corespring.conversion.qti.interactions.pie

import org.corespring.common.json.JsonUtil
import org.corespring.common.xml.XMLNamespaceClearer
import org.corespring.qti.models.QtiItem
import play.api.libs.json._

import scala.xml._
import scala.xml.transform.RewriteRule

/**
 * Base class for a transformation object which takes in QTI XML nodes and returns CoreSpring JSON+XML formats.
 */
abstract class InteractionTransformer extends RewriteRule with XMLNamespaceClearer with JsonUtil {

  def interactionJs(qti: Node, manifest: Node): Map[String, JsObject]
  /**
   * Given a node and QTI document, method looks at node's responseIdentifier attribute, and finds a
   * <responseDeclaration/> within the QTI document whose identifier attribute matches.
   */
  def responseDeclaration(node: Node, qti: Node): Node = {
    (node \ "@responseIdentifier").text match {
      case "" => throw new IllegalArgumentException("Node does not have a responseIdentifier")
      case identifier: String => {
        (qti \\ "responseDeclaration").find(n => (n \ "@identifier").text == identifier) match {
          case Some(node) => node
          case _ => throw new IllegalArgumentException(s"QTI does not contain responseDeclaration for $identifier")
        }
      }
    }
  }
  def removeNamespaces(node: Node): Node = {
    node match {
      case elem: Elem => {
        elem.copy(
          scope = TopScope,
          prefix = null,
          attributes = removeNamespacesFromAttributes(elem.attributes),
          child = elem.child.map(removeNamespaces)
        )
      }
      case other => other
    }
  }

  def removeNamespacesFromAttributes(metadata: MetaData): MetaData = {
    metadata match {
      case UnprefixedAttribute(k, v, n) => new UnprefixedAttribute(k, v, removeNamespacesFromAttributes(n))
      case PrefixedAttribute(pre, k, v, n) => new UnprefixedAttribute(k, v, removeNamespacesFromAttributes(n))
      case Null => Null
    }
  }
}