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

  }

}
