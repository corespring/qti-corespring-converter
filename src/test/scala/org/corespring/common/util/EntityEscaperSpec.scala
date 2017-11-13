package org.corespring.common.util

import org.specs2.mutable.Specification

import scala.xml.XML

class EntityEscaperSpec extends Specification with EntityEscaper {

  import EntityEscaper._

  "escapeEntities" should {

    "convert decimal &#<int>; values to entity nodes" in {
      val filtered = entities.filterNot(entity => dontEncode.contains(entity.char))

      forall(filtered) { entity =>
        escapeEntities(s"""&#${entity.unicode};""") must be equalTo (s"!!!csentity!!!${entity.unicode}!!!csendentity!!!")
      }
    }

    "convert &<string>; values to entity nodes" in {
      entities.filter{e =>
        e.name.nonEmpty && e.name != Some("lt") && e.name != Some("gt")
      }.map(entity => {
        escapeEntities(s"""&${entity.name.get};""") must be equalTo (s"!!!csentity!!!${entity.unicode}!!!csendentity!!!")
      })
    }

  }

  "unescapeEntities" should {

    "convert entity nodes to decimal values" in {

      forall(entities.filterNot(e => Seq('<', '>').contains(e.char))) { e =>
        unescapeEntities(s"!!!csentity!!!${e.unicode}!!!csendentity!!!") must be equalTo (s"&#${e.unicode};")
      }
    }

  }

  "unescapeAll" should {
    "convert &#60; to <" in (unescapeAll("&#60;") must_== "<")
    "convert &lt; to <" in (unescapeAll("&lt;") must_== "<")
    "convert &#62; to >" in (unescapeAll("&#62;") must_== ">")
    "convert &gt; to >" in (unescapeAll("&gt;") must_== ">")
  }

  "escape + xml + toString + unescape" should {

    "preserve original markup for unicode values" in {
      entities.filterNot(entity => dontEncode.contains(entity.char)).map(entity => {
        unescapeEntities(XML.loadString(escapeEntities(s"<div>&#${entity.unicode};</div>")).toString) must be equalTo (s"<div>&#${entity.unicode};</div>")
      })
      true === true
    }

    "convert entity strings to entity unicode" in {
      entities.filterNot(entity => dontEncode.contains(entity.char) || entity.name.isEmpty).map(entity => {
        unescapeEntities(XML.loadString(escapeEntities(s"<div>&${entity.name.get};</div>")).toString) must be equalTo (s"<div>&#${entity.unicode};</div>")
      })
    }

  }

}