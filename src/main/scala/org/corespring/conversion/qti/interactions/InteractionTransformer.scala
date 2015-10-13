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

  def interactionJs(qti: Node): Map[String, JsObject]

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

  // TODO: Going to be a lot of work to port this.
  def feedback(node: Node, qti: Node): JsArray = ???

  implicit class AddPrompt(interaction: Node) {

    private def prompt(node: Node): Option[String] = (node \ "prompt") match {
      case seq: NodeSeq if seq.nonEmpty => Some(seq.head.child.map(clearNamespace).mkString)
      case _ => None
    }

    /**
     * Prepends a <p/> with the prompt in it if present in the source XML
     */
    def withPrompt(node: Node): Seq[Node] = prompt(node) match {
      case Some(prompt) =>
        val promptXml = XML.loadString(String.format("<p>%s</p>", prompt))
        Seq(promptXml, interaction)
      case _ => interaction
    }
  }

}
