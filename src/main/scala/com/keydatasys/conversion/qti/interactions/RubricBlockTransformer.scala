package com.keydatasys.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json._

import scala.xml._

object RubricBlockTransformer extends InteractionTransformer {

  override def transform(node: Node, manifest: Node) = node match {
    case node: Node if (Seq("rubricBlock", "sampleBlock").contains(node.label)) => Seq.empty[Node]
    case _ => node
  }

  /**
   * TODO: Persist this as well
   */
//  def getRubric(qti: Node): Option[Resource] = {
//    (qti \\ "rubricBlock").length match {
//      case 0 => None
//      case _ => Some(Resource(
//        name = "Rubric",
//        files = Seq(new VirtualFile(
//          name = s"${(qti \\ "assessmentItem" \ "@identifier").text}-rubric.xml",
//          contentType = "text/xml",
//          isMain = true,
//          content = (qti \\ "rubricBlock").mkString))))
//    }
//  }

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = Map.empty

}
