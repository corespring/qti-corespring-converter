package com.keydatasys.conversion.qti.util

import org.specs2.mutable.Specification

import scala.xml.XML

class EntityEscaperSpec extends Specification with EntityEscaper {

  import EntityEscaper._

  "test" should {

    "be great" in {
      val test = "Read the passage from the short story &#x201C;Where Lovers Dream.&#x201D; Then answer the questions."
      println(convertHexEntities(test))
      true === false
    }

  }

}