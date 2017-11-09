package org.corespring.utils

import org.specs2.mutable.Specification

class CDataHelperTest extends Specification {


  "stripCDataAndEscapeIfNeeded" should {


    "escape brackets in regular text" in {
      val xml = """<node><![CDATA[a < b]]></node>"""
      val out = CDataHelper.stripCDataAndEscapeIfNeeded(xml)
      out must_== "<node>a &lt; b</node>"
    }

    "not escape brackets in xml content" in {
      val xml = """<node><![CDATA[<audio><source src="hi"></audio>]]></node>"""
      val out = CDataHelper.stripCDataAndEscapeIfNeeded(xml)
      out must_== """<node><audio><source src="hi" /></audio></node>"""
    }

    "allows basic html tags in mixed content" in {
      val xml = """<node><![CDATA[a <b/>]]></node>"""
      val out = CDataHelper.stripCDataAndEscapeIfNeeded(xml)
      out must_== "<node>a <b></b></node>"
    }
  }
}
