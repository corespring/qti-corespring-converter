package org.parcconline.conversion

import org.specs2.mutable.Specification

import scala.collection.immutable.TreeMap

class BootstrapTranslatorSpec extends Specification with BootstrapTranslator {

  "replaceColumns" should {

    val bootstrapClassSubstitutions = TreeMap(
      "span1" -> "col-md-1",
      "span2" -> "col-md-2",
      "span3" -> "col-md-3",
      "span4" -> "col-md-4",
      "span5" -> "col-md-5",
      "span6" -> "col-md-6",
      "span7" -> "col-md-7",
      "span8" -> "col-md-8",
      "span9" -> "col-md-9",
      "span10" -> "col-md-10",
      "span11" -> "col-md-11",
      "span12" -> "col-md-12"
    )

    def manualReplacement(html: String) = bootstrapClassSubstitutions.foldLeft(html){ case (acc, (before, after)) =>
      acc.replaceAllLiterally(before, after)
    }

    val html =
      <div class="item-body">
        <row>
          {
            bootstrapClassSubstitutions.map{ case(before, after) => {
              <div class={before}></div>
            }}
          }
        </row>
      </div>.toString

    "replace all span* with col-md-*" in {
      replaceColumns(html).stripWhitespace must beEqualTo(manualReplacement(html).stripWhitespace).ignoreCase
    }

  }

  "wrapInContainer" should {

    val content = s"""<div class="content">Great!</div>"""

    def html(content: String) = s"""<div class="item-body">$content</div>"""

    """wrap contents of <div class="item-body"/> in a <div class="container"/>""" in {
      wrapInContainer(html(content))
        .stripWhitespace must beEqualTo(html(s"""<div class="container">$content</div>""").stripWhitespace).ignoreCase
    }

  }

  "translate" should {

    val content = """<div>&#x2019;</div>"""

    "not encode entities" in {
      BootstrapTranslator.translate(content) must be equalTo(content)
    }

  }

  implicit class StripWhitespace(string: String) {

    def stripWhitespace: String = {
      val result = new StringBuilder()
      var lastWasSpace = true
      0.to(string.length-1).foreach { i =>
        val c = string.charAt(i)
        if (!Character.isWhitespace(c)) {
          result.append(c)
        }
      }
      result.toString.trim
    }

  }

}
