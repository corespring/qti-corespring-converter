import java.nio.charset.StandardCharsets

import org.corespring.common.html.JsoupParser
import org.jsoup.Jsoup
import org.jsoup.nodes.Document.OutputSettings
import org.jsoup.safety.Whitelist

import scala.xml.XML

//  <node>&radic;</node>
val xml = """<?xml version="1.0" encoding="utf-8"?>
             <root>
  <node><![CDATA[y < 4]]></node>
  <node>&#945;</node>
</root>"""


def stripCDataTags(xmlString: String) =
  """(?s)<!\[CDATA\[(.*?)\]\]>""".r.replaceAllIn(xmlString, "$1")

xml.toString()

val parsed = XML.loadString(xml)
(parsed \\ "node").map(_.text)

val outputSettings = new OutputSettings()
  .syntax(OutputSettings.Syntax.xml)
  .charset(StandardCharsets.UTF_8)

  .prettyPrint(false)
Jsoup.clean("hi there, <img>", "", Whitelist.relaxed(), outputSettings)
XML.loadString("<node>hi there</node>")
/*
val parsedTwo = XML.loadString(stripCDataTags(xml))
(parsedTwo \\ "node").map(_.text)
 */
