package org.measuredprogress.conversion.qti.interactions

import org.corespring.common.xml.NodeWithFinder
import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json.{Json, JsObject}

import scala.xml._

object RubricBlockTransformer extends InteractionTransformer with ImageConverter {

  import NodeWithFinder.NodeWithFinder

  private def isInstructions(node: Node) = node match {
    case node: Node if ("rubricBlock" == node.label) => node.attribute("view") match {
      case Some(attribute) => attribute.text match {
        case "author" => true
        case _ => false
      }
      case _ => false
    }
    case _ => false
  }

  def teacherInstructionsId(node: Node) = s"teacher-instructions-${node.hashCode()}"

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    qti.matching(isInstructions)
      .zipWithIndex.map {
      case (teacherInstructions, index) => {
        teacherInstructionsId(teacherInstructions) ->
          Json.obj(
            "componentType" -> "corespring-teacher-instructions",
            "teacherInstructions" -> <div class="teacher-instructions">{teacherInstructions.child}</div>.toString
          )
      }
    }.toMap

  override def transform(node: Node, manifest: Node) = node match {
    case node: Node if ("rubricBlock" == node.label) => node.attribute("use") match {
      case Some(attribute) => attribute.text match {
        case "stimulus" => convertObjectsToImages(<div class="stimulus">{node.child}</div>)
        case _ => Seq.empty[Node]
      }
      case _ => isInstructions(node) match {
        case true => <corespring-teacher-instructions id={ teacherInstructionsId(node) }></corespring-teacher-instructions>
        case _ => Seq.empty[Node]
      }
    }
    case node: Node if ("sampleBlock" == node.label) => Seq.empty[Node]
    case _ => node
  }
}
