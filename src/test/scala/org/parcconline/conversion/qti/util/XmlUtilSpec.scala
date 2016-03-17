package org.parcconline.conversion.qti.util

import org.specs2.mutable.Specification

class XmlUtilSpec extends Specification {

  "removeXmlDeclaration" should {

    val XMLDeclaration = """<?xml version="1.0"?>"""

    val xmlDoc = <p>Hey this is a document</p>

    "string contains XML declaration" should {

      "remove XML declaration" in {
        XmlUtil.removeXmlDeclaration(s"$XMLDeclaration${xmlDoc.toString}") must be equalTo(xmlDoc.toString)
      }

      "test" in {
        val longerOne = "  <?xml version=\"1.0\" encoding=\"UTF-8\"?><assessmentItem></assessmentItem>"
        println(XmlUtil.removeXmlDeclaration(longerOne))
        true === true
      }

    }

    "string does not contain XML declaration" should {

      "not change the string" in {
        XmlUtil.removeXmlDeclaration(xmlDoc.toString) must be equalTo(xmlDoc.toString)
      }

    }

  }

}
