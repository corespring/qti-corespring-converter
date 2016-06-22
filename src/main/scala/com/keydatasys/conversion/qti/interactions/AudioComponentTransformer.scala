package com.keydatasys.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json.{Json, JsObject}

import scala.xml.{Elem, Node}

object AudioComponentTransformer extends InteractionTransformer {

  private val reg = "(.*/)?(.*).mp3".r

  private def fileIdentifier(node: Node) = (node \ "source" \\ "@src").text match {
    case reg(_, identifier) => Some(identifier)
    case _ => None
  }

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case elem: Elem if elem.label == "audio" => fileIdentifier(node) match {
      case Some(id) => <cs-audio id={id} />
      case _ => node
    }
    case _ => node
  }

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = (qti \\ "audio").map{ node =>
    fileIdentifier(node) match {
      case Some(id) => id -> Json.obj(
        "componentType" -> "corespring-audio",
        "formats" -> Json.obj(
          "audio/mpeg" -> s"$id.mp3",
          "audio/ogg" -> s"$id.ogg"
        )
      )
      case _ => throw new IllegalStateException("Node did not match pattern")
    }
  }.toMap

}
