package org.corespring.utils



import org.slf4j.LoggerFactory
import org.corespring.macros.DescribeMacro._

import scala.util.Random
import java.nio.charset.StandardCharsets

import org.jsoup.Jsoup
import org.jsoup.nodes.Document.OutputSettings
import org.jsoup.parser.Parser

object CDataHelper {

  val logger = LoggerFactory.getLogger(CDataHelper.this.getClass)

  def stripCDataTags(xmlString: String) = """(?s)<!\[CDATA\[(.*?)\]\]>""".r.replaceAllIn(xmlString, "$1")

  private val outputSettings = new OutputSettings()
    .syntax(OutputSettings.Syntax.xml)
    .charset(StandardCharsets.UTF_8)
    .prettyPrint(false)

  /**
    * KDS passages contain invalid </video> tag XML. They are probably unaware of this because it's in CDATA, so any
    * plain old XML parser won't complain about it. When it hits the user's browser, their browser can probably render
    * it, but Scala's XML parser is a lot more strict, so we have to deal with it. Good stuff.
    */
  def fixXml(xml: String) = {
    val doc = Jsoup.parse(xml, "", Parser.xmlParser())
    doc.outputSettings(outputSettings)
    val out = doc.outerHtml()
    logger.trace(describe(out))
    out
  }


}
