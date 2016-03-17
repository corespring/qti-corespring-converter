package org.parcconline.conversion

import org.jsoup.Jsoup
import org.jsoup.nodes.Entities
import org.jsoup.nodes.Entities.EscapeMode
import scala.collection.JavaConversions._

/**
 * Created by bburton on 3/15/16.
 */
trait BootstrapTranslator {

  protected def replaceColumns(html: String): String = {
    val doc = Jsoup.parse(html)
    List.range(1, 12 + 1).foreach(n => {
      doc.getElementsByClass(s"span$n").removeClass(s"span$n").addClass(s"col-md-$n")
    })
    doc.outputSettings().charset("ascii").escapeMode(Entities.EscapeMode.extended)
    doc.select("body").html
  }

  protected def wrapInContainer(html: String): String = {
    val doc = Jsoup.parse(html)
    doc.getElementsByClass("item-body").foreach(itemBody => {
      val container = doc.createElement("div").addClass("container")
      container.html(itemBody.html)
      itemBody.html(container.outerHtml)
    })
    doc.outputSettings().charset("ascii").escapeMode(Entities.EscapeMode.extended)
    doc.select("body").html
  }

}

object BootstrapTranslator extends BootstrapTranslator {

  def translate(html: String) = replaceColumns(wrapInContainer(html))

}