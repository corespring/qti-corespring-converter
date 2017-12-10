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

  private def escapeTags(s: String): String = {
    s
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
  }


  /**
    * TODO:
    * We have an issues where by stripping the cdata tags we run the risk of having invalid xml eg:
    * <node><![CDATA[a<b]]></node> -> <node>a<b</node> <--- this is invalid xml
    * However this step is needed because there qti sometimes contains xml within the cdata that we need to parse and transform:
    * <node><![CDATA[<audio>..</audio>]]></node>
    *
    * We can't just escape < and > because doing so will turn <audio> to &lt;audio&gt; which won't get parsed.
    *
    * The best option for now is try and 1. jsoup it, 2. then parse it - it it parses then don't escape <,> - if it doesnt parse, then escape them???
    *
    * OR:
    * look for <audio ... or <video .. in the string and if present then dont escape ?
    *
    * @param xmlString
    * @return
    */
  def stripCDataAndEscapeIfNeeded(xmlString: String): String = {

    def escapeTagsIfNotXml(s: String): String = {
      val fixed = fixXml(s)
      logger.trace(describe(s))
      logger.trace(describe(fixed))
      fixed
    }

    try {

      logger.trace(describe(xmlString))

      val dollarsEscaped = xmlString.replaceAll("\\$", """\\\$""")
      """(?s)<!\[CDATA\[(.*?)\]\]>""".r.replaceAllIn(dollarsEscaped, m => {
        val contents = m.group(1)
        escapeTagsIfNotXml(contents)
      })
    } catch {
      case e: Exception => {
        val random = Random.nextInt()
        ErrorDir.dump(s"CDataHelper_${random}", Some(e), "content.xml" -> xmlString)
        throw e
      }
    }
  }


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
    /** Jsoup.clean is too aggressive - just parse and emit */
    val doc = Jsoup.parse(xml, "", Parser.xmlParser())
    doc.outputSettings(outputSettings)
    val out = doc.outerHtml()
    logger.trace(describe(out))
    out
  }


}
