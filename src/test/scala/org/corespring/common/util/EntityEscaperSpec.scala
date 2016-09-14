package org.corespring.common.util

import org.specs2.mutable.Specification

import scala.xml.XML

class EntityEscaperSpec extends Specification with EntityEscaper {

  import EntityEscaper._

  "escapeEntities" should {

    "convert decimal &#<int>; values to entity nodes" in {
      entities.filterNot(entity => dontEncode.contains(entity.char)).map(entity => {
        escapeEntities(s"""&#${entity.unicode};""") must be equalTo(s"!!!csentity!!!${entity.unicode}!!!csendentity!!!")
      })
    }

    "convert &<string>; values to entity nodes" in {
      entities.filter(_.name.nonEmpty).map(entity => {
        escapeEntities(s"""&${entity.name.get};""") must be equalTo(s"!!!csentity!!!${entity.unicode}!!!csendentity!!!")
      })
    }

  }

  "unescapeEntities" should {

    "convert entity nodes to decimal values" in {
      entities.map(entity => {
        unescapeEntities(s"!!!csentity!!!${entity.unicode}!!!csendentity!!!") must be equalTo(s"&#${entity.unicode};")
      })
    }

  }

  "escape + xml + toString + unescape" should {

    "preserve original markup for unicode values" in {
      entities.filterNot(entity => dontEncode.contains(entity.char)).map(entity => {
        unescapeEntities(XML.loadString(escapeEntities(s"<div>&#${entity.unicode};</div>")).toString) must be equalTo(s"<div>&#${entity.unicode};</div>")
      })
      true === true
    }

    "convert entity strings to entity unicode" in {
      entities.filterNot(entity => dontEncode.contains(entity.char) || entity.name.isEmpty).map(entity => {
        unescapeEntities(XML.loadString(escapeEntities(s"<div>&${entity.name.get};</div>")).toString) must be equalTo(s"<div>&#${entity.unicode};</div>")
      })
    }

  }

}