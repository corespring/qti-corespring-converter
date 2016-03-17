package com.keydatasys.conversion.qti.util

import org.specs2.mutable.Specification

import scala.xml.XML

class EntityEscaperSpec extends Specification with EntityEscaper {

  "encodeSafeEntities" should {

    "encode &quot;" in {
      val string = "this is in &quot;quotes&quot;"
      encodeSafeEntities(string) must be equalTo("""this is in "quotes"""")
    }

  }

}