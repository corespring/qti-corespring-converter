package org.corespring.conversion.qti.interactions

import scala.xml._

import play.api.libs.json._

object TexTransformer extends InteractionTransformer {

  implicit class TexConverter(child: Seq[Node]) {

    def convertToTex(implicit parent: Node): Node = {
      scala.xml.XML.loadString((parent \ "@inline").text match {
        case "false" => s"<span>$$$$${child.map(clearNamespace).mkString}$$$$</span>"
        case _ => s"<span>\\(${child.map(clearNamespace).mkString}\\)</span>"
      })
    }

  }

  override def transform(node: Node, manifest: Node) = {
    implicit val parent = node
    node match {
      case elem: Elem if elem.label == "tex" => elem.child.convertToTex
      case _ => node
    }
  }

  override def interactionJs(qti: Node, manifest: Node) = Map.empty[String, JsObject]

}
