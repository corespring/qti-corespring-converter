package org.parcconline.conversion.qti.util

import org.corespring.common.file.SourceWrapper
import org.parcconline.conversion.qti.interactions.SVGZWriter
import org.specs2.mutable.Specification
import play.api.libs.json.JsString

class SVGZConverterSpec extends Specification {

  val svgz = {
    val filename = "M41422_A.svgz"
    new SourceWrapper(filename, getClass().getResourceAsStream(s"/$filename"))
  }

  "rewriteUrls" should {

    def stringWithExtension(extension: String) = s"""<img src="yas-queen.$extension"/><img src="tacocat.$extension"/>"""

    "replace all instances of .svgz with .png" in {
      val input = stringWithExtension("svgz")
      SVGZWriter.rewriteUrls(JsString(input)).as[String] must be equalTo(stringWithExtension("png"))
    }

  }

}
