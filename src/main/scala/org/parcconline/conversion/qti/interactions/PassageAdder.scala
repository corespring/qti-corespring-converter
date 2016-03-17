package org.parcconline.conversion.qti.interactions

import java.nio.charset.StandardCharsets

import com.keydatasys.conversion.qti.util.EntityEscaper
import org.corespring.common.file.SourceWrapper
import org.corespring.conversion.qti.interactions.InteractionTransformer
import org.jsoup.Jsoup
import org.jsoup.nodes.Document.OutputSettings
import org.jsoup.nodes.Entities
import org.jsoup.safety.Whitelist
import org.parcconline.conversion.qti.util.XmlUtil
import play.api.libs.json.JsObject

import scala.xml._

class PassageAdder(sources: Map[String, SourceWrapper]) extends InteractionTransformer with EntityEscaper {

  private def cleanSoup(xml: String) =
    Jsoup.clean(xml, "", Whitelist.none,
      new OutputSettings().syntax(OutputSettings.Syntax.xml).charset(StandardCharsets.UTF_8)
        .charset("ascii").escapeMode(Entities.EscapeMode.extended))

  private def isPassage(node: Elem) =
    node.label == "include" && node.copy(child = Seq.empty).toString.contains("xi:include")

  private def hasPassageClass(node: Elem) = (node \ "@class").text.contains("passage")

  private def passageMarkup(node: Elem): Option[Node] = isPassage(node) match {
    case true => {
      val filename = (node \ "@href").text
      sources.get(filename) match {
        case Some(passageFile) => {
          val doc = Jsoup.parse(cleanSoup(passageFile.getLines.mkString))
          doc.outputSettings().charset("ascii").escapeMode(Entities.EscapeMode.extended)
          Some(XML.loadString(escapeEntities(s"<div>${doc.select("body").html}</div>")))
        }
        case _ => None
      }
    }
    case _ => None
  }

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = Map.empty[String, JsObject]

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case elem: Elem if (isPassage(elem)) => {
      passageMarkup(elem) match {
        case Some(passageNode) => passageNode
        case _ => throw new IllegalStateException("Could not find passage in provided sources")
      }
    }
    case elem: Elem if (hasPassageClass(elem)) => elem.addAttribute("show", "true")
    case _ => node
  }

  implicit class AddAttribute(elem: Elem) {

    def addAttribute(key: String, value: String) = elem % new UnprefixedAttribute(key, value, Null)

  }

}
