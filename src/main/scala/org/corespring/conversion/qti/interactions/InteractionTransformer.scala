package org.corespring.conversion.qti.interactions

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
  def transform(node: Node, manifest: Node): Seq[Node]

  def transform(ns: Seq[Node], manifest: Node): Seq[Node] = {
    val changed = ns.flatMap(node => transform(node, manifest))
    if (changed.length != ns.length || (changed, ns).zipped.exists(_ != _)) changed
    else ns
  }

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

  def feedback(node: Node, qti: Node): JsArray = {
    val qtiItem = QtiItem(qti)
    val choiceIds: Seq[Node] = Seq("simpleChoice", "inlineChoice", "feedbackInline").map(node \\ _).map(_.toSeq).flatten
    val componentId = (node \ "@responseIdentifier").text.trim

    val feedbackObjects: Seq[JsObject] = choiceIds.map { (n: Node) =>

      val id = (n \ "@identifier").text.trim
      val fbInline = qtiItem.getFeedback(componentId, id)

      fbInline.map { fb =>
        val content = if (fb.defaultFeedback) {
          fb.defaultContent(qtiItem)
        } else {
          fb.content
        }
        Json.obj("value" -> id, "feedback" -> content)
      }
    }.flatten.distinct
    JsArray(feedbackObjects)
  }

  private def getFeedback(qti: Node, id: String, value: String): Option[JsObject] = {

    (qti \\ "responseDeclaration").find(rd => (rd \ "@identifier").text == id) match {
      case Some(feedback) => Some(Json.obj())
      case _ => None
    }
  }

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


trait Transformer {

  def transform(qti: Node, manifest: Node): Node

}