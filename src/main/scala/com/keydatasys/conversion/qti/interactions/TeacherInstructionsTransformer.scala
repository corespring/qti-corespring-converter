package com.keydatasys.conversion.qti.interactions

import org.corespring.common.util.XHTMLCleaner
import org.corespring.common.xml.NodeWithFinder
import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json._
import scala.xml._

object TeacherInstructionsTransformer extends InteractionTransformer with XHTMLCleaner {

  import NodeWithFinder.NodeWithFinder

  val isInstructions: Node => Boolean = n =>
    ((n.label == "partBlock") && Seq("@label", "@identifier").find(a => (n \ a).text == "teacherInstructions").nonEmpty) ||
      (n.label == "teacherInstructions")

  def teacherInstructionsId(node: Node) = s"teacher-instructions-${node.hashCode()}"

  override def transform(node: Node, manifest: Node) = node match {
    case node: Node if (isInstructions(node)) =>
      <corespring-teacher-instructions id={ teacherInstructionsId(node) }></corespring-teacher-instructions>
    case _ => node
  }

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    qti.matching(isInstructions)
      .zipWithIndex.map {
      case (teacherInstructions, index) => {
        teacherInstructionsId(teacherInstructions) ->
          Json.obj(
            "componentType" -> "corespring-teacher-instructions",
            "teacherInstructions" -> teacherInstructions.convertNonXHTMLElements.map(_.text)
              .getOrElse(throw new Exception("Teacher instructions could not be converted")))
      }
    }.toMap



}
