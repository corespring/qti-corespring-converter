package org.measuredprogress.conversion.qti.interactions

import org.specs2.mutable.Specification


class ImageConverterTest extends Specification with ImageConverter{

  /**
    * We were converting in html mode which was modifying the markup as per the html spec.
    * We have updated to parse in xml mode, this test asserts that no artifact is introduced when running `convertHtml`
    * https://github.com/jhy/jsoup/issues/436
    */
  "converts html and doesn't add artifacts" in {
    val htmlTwo = "<p><strong><b>B</b></strong><div>div</div></p>"
    val out = convertHtml(htmlTwo)
    out.replace("\n", "").replace(" ", "") must_== htmlTwo
  }
}
