package com.keydatasys.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.corespring.conversion.qti.transformers.InteractionRuleTransformer
import org.specs2.mutable.Specification

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
      (xhtml \\ "corespring-audio" \ "@id").text must be equalTo(id)
    }

    "not contain <audio/> tag" in {
      (xhtml \\ "audio") must beEmpty
    }

  }

  "interactionJs" should {

    val result =
      AudioComponentTransformer.interactionJs(qti, QTIManifest.EmptyManifest).get(id).getOrElse(throw new Exception("Oops"))

    "have weight" in ((result \ "weight").asOpt[Int] must_== Some(0))
    "have componentType" in ((result \ "componentType").asOpt[String] must_== Some("corespring-audio"))
    "have filename" in ((result \ "fileName").asOpt[String] must_== Some(s"$id.mp3"))
    "have pauseButtonLabel" in ((result \ "pauseButtonLabel").asOpt[String] must_== Some("Stop"))
    "have playButtonLabel" in ((result \ "playButtonLabel").asOpt[String] must_== Some("Listen"))
    "have title" in ((result \ "title").asOpt[String] must_== Some("Audio"))
    "have ui" in ((result \ "ui").asOpt[String] must_== Some("fullControls"))

  }

}
