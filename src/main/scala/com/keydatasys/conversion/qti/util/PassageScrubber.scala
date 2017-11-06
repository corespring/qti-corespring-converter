package com.keydatasys.conversion.qti.util

import java.nio.charset.StandardCharsets

import org.jsoup.Jsoup
import org.jsoup.nodes.Document.OutputSettings
import org.jsoup.safety.Whitelist

trait PassageScrubber {

  val whitelist = Whitelist.relaxed()
    .addTags("video", "audio", "source")
    .addAttributes("video", "width", "height", "controls")
    .addAttributes("audio", "controls")
    .addAttributes("source", "src", "type")

  val outputSettings = new OutputSettings()
    .syntax(OutputSettings.Syntax.xml)
    .charset(StandardCharsets.UTF_8)
    .prettyPrint(false)

  /**
   * KDS passages contain invalid </video> tag XML. They are probably unaware of this because it's in CDATA, so any
   * plain old XML parser won't complain about it. When it hits the user's browser, their browser can probably render
   * it, but Scala's XML parser is a lot more strict, so we have to deal with it. Good stuff.
   */
  def scrub(xml: String) = Jsoup.clean(xml, "", whitelist, outputSettings)

}
