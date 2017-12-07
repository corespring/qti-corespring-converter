package org.corespring.conversion.qti

class Item665010Spec extends BaseRunner {


  override def sourceId = "665010"

  "it creates a table" in {


    (playerDefJson \ "xhtml").as[String] must_== "<div></div>"
  }
}
