package org.corespring.conversion.qti

import org.specs2.mutable.Specification
import play.api.libs.json.JsObject

import scala.xml.XML

class QtiTransformerTest extends Specification {
  private def loadXml(path:String) = {
    val url = this.getClass.getResource(path)
    require(url != null)
    XML.load(url)
  }

 "transform" should {
   "transform bad qti found on qa" in {
     val xml = loadXml("/qti-xml/one.xml")
     val json = QtiTransformer.transform(xml)
     (json \ "components").asOpt[JsObject].isDefined === true
   }
 }

}
