package com.keydatasys.conversion.qti.interactions

import org.corespring.conversion.qti.NodeAndJsonTransformer
import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json.{JsObject, Json}

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node}

/**
  * Some nodes contain CDATA within which is an <audio> tag.
  * expose these in the markup.
  * object ExtractAudioFromText extends RewriteRule {
  * *
  * override def transform(n: Node) = {
  * *
  * n match {
  * case e: Elem if e.text.indexOf("<audio") != -1 => {
  * val stripped = CDataHelper.stripCDataTags(e.text)
  * val empty = e.copy(child = Seq(<extract_audio_contents_here/>))
  * val xmlMarkup = CDataHelper.fixXml(empty.toString.replaceFirst("<extract_audio_contents_here/>", stripped))
  * val p = ConstructingParser.fromSource(Source.fromString(xmlMarkup), false)
  *p.document().docElem
  * }
  * case _ => n
  * }
  * }
  * }
  */

object AudioMarkupAndComponent extends NodeAndJsonTransformer {

  private val reg = "(.*/)?(.*).mp3".r

  private def fileIdentifier(node: Node) = (node \ "source" \\ "@src").text match {
    case reg(_, identifier) => Some(identifier)
    case _ => None
  }

  private val rule = new RewriteRule {
    override def transform(node: Node): Seq[Node] = node match {
      case elem: Elem if elem.label == "audio" => fileIdentifier(node) match {
        case Some(id) => <corespring-audio id={id}></corespring-audio>
        case _ => node
      }
      case _ => node
    }
  }

  def interactionJs(qti: Node): Map[String, JsObject] = (qti \\ "audio").map { node =>
    fileIdentifier(node) match {
      case Some(id) => id -> Json.obj(
        "weight" -> 0,
        "componentType" -> "corespring-audio",
        "fileName" -> s"$id.mp3",
        "pauseButtonLabel" -> "Stop",
        "playButtonLabel" -> "Listen",
        "title" -> "Audio",
        "ui" -> "fullControls"
      )
      case _ => throw new IllegalStateException("Node did not match pattern")
    }
  }.toMap

  override def transform(n: Elem, json: Map[String, JsObject]): (Elem, Map[String, JsObject]) = {
    (
      new RuleTransformer(rule).transform(n).head.asInstanceOf[Elem],
      json ++ interactionJs(n)
    )
  }
}

object AudioComponentTransformer extends InteractionTransformer {

  private val reg = "(.*/)?(.*).mp3".r

  private def fileIdentifier(node: Node) = (node \ "source" \\ "@src").text match {
    case reg(_, identifier) => Some(identifier)
    case _ => None
  }

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case elem: Elem if elem.label == "audio" => fileIdentifier(node) match {
      case Some(id) => <corespring-audio id={id}></corespring-audio>
      case _ => node
    }
    case _ => node
  }

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = (qti \\ "audio").map { node =>
    fileIdentifier(node) match {
      case Some(id) => id -> Json.obj(
        "weight" -> 0,
        "componentType" -> "corespring-audio",
        "fileName" -> s"$id.mp3",
        "pauseButtonLabel" -> "Stop",
        "playButtonLabel" -> "Listen",
        "title" -> "Audio",
        "ui" -> "fullControls"
      )
      case _ => throw new IllegalStateException("Node did not match pattern")
    }
  }.toMap

}
