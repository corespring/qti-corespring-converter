package org.corespring.utils


import org.specs2.mutable.Specification
class CDataHelperTest extends Specification {


  def escapes(xml:String, expected:String) = {
    val out = CDataHelper.stripCDataAndEscapeIfNeeded(xml)
    out== expected
  }


  "stripCDataAndEscapeIfNeeded" should {


    "escape brackets in regular text" in {
      escapes( """<node><![CDATA[a < b]]></node>""", "<node>a &lt; b</node>")
    }

    "not escape brackets in xml content" in {
      escapes("""<node><![CDATA[<audio><source src="hi"></audio>]]></node>""",
       """<node><audio><source src="hi" /></audio></node>""")
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
