package org.corespring.conversion.qti


class BadMarkupSpec extends BaseRunner {
  override def sourceId = "663934"

  "kds --sourceId 663934" should {

    val playerDefJson = playerDef.map(json(zip, _)).getOrElse {
      throw new RuntimeException("Not defined")
    }

    "it works" in {
      val xhtml = (playerDefJson \ "xhtml").as[String]
      logger.info(s"xhtml string: $xhtml")
      xhtml.indexOf("<b class=\"frac\">(</b></strong>") > -1 must_== true
    }
  }
}
