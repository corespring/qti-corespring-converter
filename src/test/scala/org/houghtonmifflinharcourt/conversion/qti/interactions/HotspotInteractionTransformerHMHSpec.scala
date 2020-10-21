package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json.Reads._

class HotspotInteractionTransformerHMHSpec extends Specification {

  "HotspotInteractionTransformer" should {

    val responseIdentifier = "1"
    val correctResponse = "B"
    val choices = Map("A" -> "choice one", "B" -> "choice two","C" -> "choice three","D" -> "choice four")
    val promptHS =  s"Three of these statements describe possible outcomes of this demonstration.";
    val shuffle = false

    def textEntryQti(responseIdentifier: String = responseIdentifier,
                        correctResponse: String = correctResponse,
                        promptHS: String = promptHS,
                        choices: Map[String, String] = choices,
                        shuffle: Boolean = shuffle
                       ) =
      <assessmentItem xmlns="http://www.imsglobal.org/xsd/imsqti_v2p1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:m="http://www.w3.org/1998/Math/MathML" xsi:schemaLocation="http://www.imsglobal.org/xsd/imsqti_v2p1 http://www.imsglobal.org/xsd/qti/qtiv2p1/imsqti_v2p1p2.xsd http://www.w3.org/1998/Math/MathML http://www.w3.org/Math/XMLSchema/mathml2/mathml2.xsd" identifier="IMNL20E11_chk_rp_chng_unk_prb_ob_QUESTION_270148531_1" title="Module 05, Lesson 02, Check Understanding Q01" timeDependent="false" adaptive="false">
        <responseDeclaration identifier="RESPONSE" cardinality="single" baseType="identifier">
          <correctResponse>
            <value>OPTION_0</value>
          </correctResponse>
        </responseDeclaration>
        <outcomeDeclaration identifier="SCORE" cardinality="single" baseType="float"/>
        <itemBody>
          <hotspotInteraction responseIdentifier="RESPONSE" maxChoices="1">
            <prompt>
              <div>
                <p><strong>This is an item with more than one part. Copy your answer to this part for use in the other parts.</strong></p>
                <p>There are 7 dogs at a dog park. Some more dogs come. Now there are 15 dogs. How many more dogs come?</p>
                <p><strong>Part A</strong>  Click on the one picture that shows the problem.</p>
              </div>
            </prompt>
            <object type="image/png" data="721481.gif"/>
            <hotspotChoice identifier="OPTION_3" shape="rect" height="140" width="130" coords="1,1"/>
            <hotspotChoice identifier="OPTION_4" shape="rect" height="140" width="130" coords="140,1"/>
            <hotspotChoice identifier="OPTION_0" shape="poly" coords="2,3.975,2.2,92.175,47.4,92.175,47.4,2.775,2,3.975"/>
            <hotspotChoice identifier="OPTION_1" shape="poly" coords="51.6,2.175,51.6,92.175,97.8,92.175,97.6,0.375,51.6,2.175"/>
          </hotspotInteraction>
        </itemBody>
        <responseProcessing template="http://www.imsglobal.org/question/qtiv2p1/rptemplates/match_correct.xml"/>
      </assessmentItem>
    val hotspotInteractionResult = HotspotInteractionTransformer.interactionJs(textEntryQti(), QTIManifest.EmptyManifest)

      "transform prompt in Hotspot Interaction" in {
      val json = hotspotInteractionResult.values.headOption.getOrElse(throw new Exception("There was no result"))
      val prompt = (json \\ "markup")(1).as[String]
      prompt.contains(promptHS)
    }
  }
}