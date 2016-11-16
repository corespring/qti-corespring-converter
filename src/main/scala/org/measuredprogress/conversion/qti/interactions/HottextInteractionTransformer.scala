package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.{HottextInteractionTransformer => CoreSpringHottextInteractionTransformer}
import org.jsoup.Jsoup
import org.jsoup.nodes.{TextNode, Node => JNode}

import scala.xml.Node

import scala.collection.JavaConversions._

class HottextInteractionTransformer extends CoreSpringHottextInteractionTransformer {

  def startsWithPunctuation(node: JNode) = {
    val punctuation = Seq(",", ";", ".")
    Option(node) match {
      case Some(e: TextNode) => punctuation.find(p => e.text.startsWith(p)).nonEmpty
      case _ => false
    }
  }

  override def passage(node: Node) = {
    val doc = Jsoup.parse(node.child.mkString)
    doc.getElementsByTag("hottext").foreach(hottext => {
      val csToken = doc.createElement("span")
      csToken.addClass("cs-token")
      csToken.html(hottext.html)
      hottext.replaceWith(csToken)
      if (!startsWithPunctuation(csToken.nextSibling())) {
        csToken.after(new TextNode(" ", ""))
      }
    })
    doc.getElementsByTag("object").filter(_.attr("type").contains("image")).foreach(objectEl => {
      val imgEl = doc.createElement("img")
      imgEl.attr("src", objectEl.attr("data"))
      Seq("height", "width").filter(attr => objectEl.attr(attr).nonEmpty).foreach(attr => {
        imgEl.attr(attr, objectEl.attr(attr))
      })
      objectEl.replaceWith(imgEl)
    })
    doc.select("body").html
  }
}
