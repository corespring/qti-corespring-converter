package org.measuredprogress.conversion.qti.util

import org.corespring.common.html.JsoupParser
import org.jsoup.Jsoup
import org.jsoup.nodes.Document.OutputSettings
import org.jsoup.nodes.Entities
import play.twirl.api.TemplateMagic.javaCollectionToScala

trait NamespaceStripper {

  def stripNamespaces(string: String): String = {
    val doc = JsoupParser.parse(string)
    val namespacedTagname = ".*:(.*)".r
    doc.getAllElements.foreach{ el =>
      el.tagName match {
        case namespacedTagname(tagname) => {
          el.tagName(tagname)
        }
        case _ => {}
      }
      el.removeAttr("xmlns")
    }
    doc.select("body").html
  }

}
