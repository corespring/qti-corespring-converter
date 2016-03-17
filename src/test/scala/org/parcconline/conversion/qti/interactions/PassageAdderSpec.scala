package org.parcconline.conversion.qti.interactions

import java.io.File

import org.corespring.common.file.SourceWrapper
import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification

import scala.xml.XML

class PassageAdderSpec extends Specification with Mockito {

  val passageHref = "my-passage.xml"
  val passageContent =
    Seq("<p>This is a passage. Passage passage passage.</p>","<p>Answer those questions, please.</p>")

  val passageFile = mock[SourceWrapper]
  passageFile.getLines returns passageContent.toIterator

  val sources = Map(passageHref -> passageFile)

  val passageAdder = new PassageAdder(sources)

  "transform" should {

    "on a <xi:include/> node" should {

      val node = <xi:include href={passageHref}></xi:include>

      "return <xi:include/> node's href contents wrapped in <div/>" in {
        passageAdder.transform(node, QTIManifest.EmptyManifest) must be equalTo(XML.loadString(s"<div>${passageContent.mkString}</div>"))
      }

    }

    "on a .passage node" should {

      val node = <div class="passage">This is a passage for some item</div>

      """add attribute "show" = "true"""" in {
        (passageAdder.transform(node, QTIManifest.EmptyManifest).head \ "@show").text must beEqualTo("true")
      }

    }

  }

}
