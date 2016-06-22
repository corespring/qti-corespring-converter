package com.keydatasys.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.HottextInteractionTransformer
import org.corespring.conversion.qti.manifest.QTIManifest
import org.corespring.conversion.qti.transformers.InteractionRuleTransformer
import org.specs2.mutable.Specification
import play.api.libs.json.JsObject

import scala.xml.XML

class AudioComponentTransformerSpec extends Specification {

  val id = "676195s"

  val qti = <assessmentItem>
    <itemBody>
      <audio>
        <source src={s"./audio/$id.mp3"} type="audio/mpeg"/>
      </audio>
    </itemBody>
  </assessmentItem>


  "transform" should {

    val xhtml = XML.loadString(new InteractionRuleTransformer(AudioComponentTransformer).transform(qti).mkString)

    "contain <cs-audio/> tag with proper id" in {
      (xhtml \\ "cs-audio" \ "@id").text must be equalTo(id)
    }

    "not contain <audio/> tag" in {
      (xhtml \\ "audio") must beEmpty
    }

  }

  "interactionJs" should {

    val result =
      AudioComponentTransformer.interactionJs(qti, QTIManifest.EmptyManifest).get(id).getOrElse(throw new Exception("Oops"))

    "formats" should {
      val formats = (result \ "formats").as[JsObject]

      "contain 'audio/mpeg' and 'audio/ogg' formats for identifier" in {
        (formats \ "audio/mpeg").as[String] must be equalTo(s"$id.mp3")
        (formats \ "audio/ogg").as[String] must be equalTo(s"$id.ogg")
      }

    }

  }

}
