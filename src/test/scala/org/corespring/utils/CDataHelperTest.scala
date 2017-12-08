package org.corespring.utils

import org.specs2.mutable.Specification

class CDataHelperTest extends Specification {


  def escapes(xml:String, expected:String) = {
    val out = CDataHelper.stripCDataAndEscapeIfNeeded(xml)
    out must_== expected
  }


  "stripCDataAndEscapeIfNeeded" should {


    "escape brackets in regular text" in {
      escapes( """<node><![CDATA[a < b]]></node>""", "<node>a &lt; b</node>")
    }

    "not escape brackets in xml content" in {
      escapes("""<node><![CDATA[<audio controls=""><source src="hi"></audio>]]></node>""",
        """<node><audio controls=""><source src="hi" /></audio></node>""")
    }

    "fix invalid attributes in markup" in {
      escapes("""<node><![CDATA[<audio controls><source src="hi"></audio>]]></node>""",
        """<node><audio controls=""><source src="hi" /></audio></node>""")
    }

    "allows any attributes to be added to a node" in {

      escapes(
        """<node><![CDATA[
          |<video width="100" height="100" foo="foo" bar="bar">
          |  <source baz>
          |</video>
          |]]></node>""".stripMargin, """<node>
                                        |<video width="100" height="100" foo="foo" bar="bar">
                                        |  <source baz="">
                                        |</source></video>
                                        |</node>""".stripMargin)
    }

    "allows basic html tags in mixed content" in {
      escapes( """<node><![CDATA[a <b/>]]></node>""", "<node>a <b></b></node>")
    }

    "escapes xml with dollar in it" in {
      escapes("""<root><![CDATA[<p>$260</p>]]></root>""",  "<root><p>$260</p></root>")
    }

    "escape string with dollar in it" in {
      escapes("""<root><![CDATA[$260 < $300]]></root>""",  "<root>$260 &lt; $300</root>")
    }
  }
}