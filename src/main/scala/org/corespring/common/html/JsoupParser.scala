package org.corespring.common.html

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document}
import org.jsoup.nodes.Document.OutputSettings
import org.jsoup.parser.Parser

object JsoupParser {

  def parse(string: String): Document = Jsoup.parse(string).outputSettings(new OutputSettings().syntax(OutputSettings.Syntax.xml))

  /**
    * https://github.com/jhy/jsoup/issues/436
    *
    * @param string
    * @return
    */
  def parseXml(string: String): Document = Jsoup.parse(string, "", Parser.xmlParser()).outputSettings(new OutputSettings().syntax(OutputSettings.Syntax.xml))
}
