package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.{OrderInteractionTransformer => SuperOrderingInteractionTransformer}
import org.measuredprogress.conversion.qti.util.NamespaceStripper
import play.api.libs.json._

import scala.xml.Node

object OrderingInteractionTransformer extends SuperOrderingInteractionTransformer with NamespaceStripper {

  override def choices(node: Node) = Some(JsArray((node \\ "simpleChoice")
    .map(choice => Json.obj(
      "label" -> stripNamespaces(choice.child.filter(_.label != "feedbackInline").mkString.trim),
      "value" -> (choice \ "@identifier").text,
      "content" -> stripNamespaces(choice.child.filter(_.label != "feedbackInline").mkString.trim),
      "id" -> (choice \ "@identifier").text,
      "moveOnDrag" -> true))))

}
