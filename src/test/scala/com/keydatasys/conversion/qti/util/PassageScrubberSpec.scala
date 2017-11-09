package com.keydatasys.conversion.qti.util

import org.slf4j.LoggerFactory
import org.specs2.mutable.Specification

import scala.xml.XML

class PassageScrubberSpec extends Specification with PassageScrubber {

  val logger = LoggerFactory.getLogger(this.getClass)

  val badMarkup =
    """<video width="500" height="500" controls>
        <source src="./videos/662604p3.mp4" type="video/mp4">
        <span class="noprint">Your browser does not support the video tag.</span>
      </video>
    """

  "fixXml" in {
    val xml = """<audio><source src="hi"></audio>"""
    val out = PassageScrubber.fixXml(xml)
    out must_== """<audio><source src="hi" /></audio>"""
  }

  "scrub" should {

    val result = scrub(badMarkup)

    logger.debug(s"result: $result")



    "produce valid markup" in {
        val xml = XML.loadString(result)
      xml.label must_== "video"
    }

    "produce valid markup" in {

      val raw = """<video width="500" height="500" controls="">
                       |  <source src="./videos/662653p3.mp4" type="video/mp4">
                       |  <span class="kds-noprint">Your browser does not support the video tag.</span>
                       |</video>""".stripMargin

      val scrubbed = scrub(raw)
      logger.debug(s"scrubbed: $scrubbed")

      val xml  =XML.loadString(scrubbed)
      xml.label must_== "video"
      (xml \ "@width").text must_== "500"
      (xml \ "@height").text must_== "500"
    }

    "matches multiple tags" in {

      val raw =
        """<div tag="foo">
          |<video width="100"><source src="bar.mp4"></video>
          |<video width="100"><source src="baz.mp4"></video>
          |<audio><source src="audio-foo.mp3"></audio>
          |</div>
        """.stripMargin

      logger.debug(s"raw: $raw")
      val scrubbed = scrub(raw)
      logger.debug(s"scrubbed: $scrubbed")
      val xml = XML.loadString(scrubbed)
      (xml \\ "video").length must_== 2
      (xml \ "@tag").text must_== "foo"
      (xml \\ "source").map( n => (n \ "@src").text) must_== Seq("bar.mp4", "baz.mp4", "audio-foo.mp3")
    }
  }

}
