package org.measuredprogress.conversion.qti.interactions

import org.corespring.common.html.JsoupParser
import org.corespring.common.util.EntityEscaper
import org.corespring.conversion.qti.interactions.{HottextInteractionTransformer => CoreSpringHottextInteractionTransformer}
import org.jsoup.Jsoup
import org.jsoup.nodes.{TextNode, Node => JNode}
import org.measuredprogress.conversion.qti.util.NamespaceStripper

import scala.xml.Node

import scala.collection.JavaConversions._

class HottextInteractionTransformer extends CoreSpringHottextInteractionTransformer with NamespaceStripper with ImageConverter with EntityEscaper {

  def startsWithPunctuation(node: JNode) = {
    val punctuation = Seq(",", ";", ".")
    Option(node) match {
      case Some(e: TextNode) => punctuation.find(p => e.text.startsWith(p)).nonEmpty
      case _ => false
    }
  }

  override def passage(node: Node) = {
    val converted = convertObjectsToImages(node)
    val doc = JsoupParser.parse(converted.mkString)
    doc.getElementsByTag("hottext").foreach(hottext => {
      val csToken = doc.createElement("span")
      csToken.addClass("cs-token")
      csToken.html(escapeEntities(stripNamespaces(escapeMathML(unescapeEntities(hottext.html)))))
      hottext.replaceWith(csToken)
      if (!startsWithPunctuation(csToken.nextSibling())) {
        csToken.after(new TextNode(" ", ""))
      }
    })
    stripNamespaces(doc.select("body").html)
  }

  private def escapeMathML(string: String) = {
    val chars = Map(
      "&#60;" -> "<",
      "&#62;" -> ">",
      "&#38;" -> "&"
    )

    chars.foldLeft(string){ case(acc, (code, replacement)) => {
      acc.replaceAll(code, replacement)
    }}
  }

}
