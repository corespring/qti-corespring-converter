package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json.Reads._

class MatchInteractionTransformerHMHSpec extends Specification {

  "MatchInteractionTransformer" should {

    val promptMI =  s"Three of these statements describe possible outcomes of this demonstration.";

    var matchInteractionQti = <assessmentItem xmlns="http://www.imsglobal.org/xsd/imsqti_v2p1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.imsglobal.org/xsd/imsqti_v2p1  http://www.imsglobal.org/xsd/qti/qtiv2p1/imsqti_v2p1p2.xsd" identifier="LTNA20_G06_ST_OA_019_QUESTION_877112979" title="Views on Zoos Selection Test Q09" timeDependent="false" adaptive="false">
      <responseDeclaration identifier="RESPONSE" cardinality="multiple" baseType="directedPair">
        <correctResponse>
          <value>STEM_0 OPTION_1</value>
          <value>STEM_1 OPTION_0</value>
          <value>STEM_2 OPTION_0</value>
        </correctResponse>
      </responseDeclaration>
      <outcomeDeclaration identifier="SCORE" cardinality="single" baseType="float"/>
      <itemBody>
        <matchInteraction label="RESPONSE" responseIdentifier="RESPONSE" shuffle="false" maxAssociations="3" minAssociations="3">
          { promptMI }
          <simpleMatchSet>
            <simpleAssociableChoice identifier="STEM_0" matchMax="1">Sonia’s Blog</simpleAssociableChoice>
            <simpleAssociableChoice identifier="STEM_1" matchMax="1">Association of Zoos and Aquariums</simpleAssociableChoice>
            <simpleAssociableChoice identifier="STEM_2" matchMax="1">Innocent and Imprisoned</simpleAssociableChoice>
          </simpleMatchSet>
          <simpleMatchSet>
            <simpleAssociableChoice identifier="OPTION_0" matchMax="1">Fact-based</simpleAssociableChoice>
            <simpleAssociableChoice identifier="OPTION_1" matchMax="1">Opinion-based</simpleAssociableChoice>
          </simpleMatchSet>
        </matchInteraction>
      </itemBody>
      <responseProcessing template="http://www.imsglobal.org/question/qtiv2p1/rptemplates/match_correct.xml"/>
    </assessmentItem>
    val matchInteractionResult = MatchInteractionTransformer.interactionJs(matchInteractionQti, QTIManifest.EmptyManifest)

    "transform prompt in Match Interaction" in {
      val json = matchInteractionResult.values.headOption.getOrElse(throw new Exception("There was no result"))
      val prompt = (json \\ "prompt")(0).as[String]
      prompt must be equalTo (promptMI)
    }
  }
}