package org.corespring.common.html

import org.jsoup.Jsoup
import org.jsoup.nodes.{Entities, Document}
import org.jsoup.nodes.Document.OutputSettings

object JsoupParser {

  def parse(string: String): Document = Jsoup.parse(string).outputSettings(new OutputSettings().syntax(OutputSettings.Syntax.xml))

}
