package com.keydatasys.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json._
import scala.xml._

object ProtractorWidgetTransformer extends InteractionTransformer {

  private val id = "PROTRACTOR"

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = hasProtractor(manifest) match {
    case true => protractorJson
    case false => Map.empty
  }

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case elem: Elem if (elem.label == "itemBody" && hasProtractor(manifest)) =>
      elem.copy(child = protractorNode ++ elem.child)
    case _ => node
  }

  def protractorJson = Map(id -> Json.obj(
    "weight" -> 0,
    "clean" -> true,
    "title" -> "Protractor",
    "componentType" -> "corespring-protractor",
    "isTool" -> true,
    "noEdit" -> true,
    "model" -> Json.obj(
      "config" -> Json.obj())))

  def protractorNode = <corespring-protractor id={ id }></corespring-protractor><br/><br/>

  private def hasProtractor(manifest: Node) =
    (manifest \\ "mathTools" \ "mathTool").map(_.text.toLowerCase).contains("protractor")

}
