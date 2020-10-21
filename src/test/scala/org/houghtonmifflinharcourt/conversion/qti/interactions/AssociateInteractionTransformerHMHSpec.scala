package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json.Reads._

class AssociateInteractionTransformerHMHSpec extends Specification {

  "AssociateInteractionTransformer" should {

    val promptAI =  s"Three of these statements describe possible outcomes of this demonstration.";

    var associateInteractionQti = <assessmentItem xmlns="http://www.imsglobal.org/xsd/imsqti_v2p1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:m="http://www.w3.org/1998/Math/MathML" xsi:schemaLocation="http://www.imsglobal.org/xsd/imsqti_v2p1 http://www.imsglobal.org/xsd/qti/qtiv2p1/imsqti_v2p1p2.xsd http://www.w3.org/1998/Math/MathML http://www.w3.org/Math/XMLSchema/mathml2/mathml2.xsd" identifier="IMNL20E11_rtch_Rp_Rst_Un_ObD_QUESTION_281188162_2" title="Module 05, Lesson 01 - Restored, Reteach Q04" timeDependent="false" adaptive="false">
      <responseDeclaration identifier="RESPONSE" cardinality="multiple" baseType="pair">
        <correctResponse>
          <value>STEM_0 OPTION_1</value>
        </correctResponse>
        <mapping defaultValue="0">
          <mapEntry mapKey="STEM_0 OPTION_1" mappedValue="1"/>
        </mapping>
      </responseDeclaration>
      <outcomeDeclaration identifier="SCORE" cardinality="single" baseType="float"/>
      <itemBody>
        <div>
          <p><strong>Part B</strong></p>
          <p>Dolly has 15 crayons. She finds 3 more crayons.</p>
          <p>How many crayons does she have now?</p>
          <p>Show your work by completing the drawing.</p>
          <p>Please ask your teacher for help to answer this question.</p>
          <p>Choose each group and equation for the correct box in the table. You will use all of the answer choices.</p>
        </div><div>
        <p><img src="650669.gif" alt="Three rows of counters are shown. Each row has five counters."/></p>
        <p><strong>Hint(s):</strong> <em>The counters show one number from the word problem. Find the group of counters that shows the other number.<br/>The number of counters shown plus the number of counters you add should equal the correct sum in the equation from Part A.</em></p></div>
        <associateInteraction responseIdentifier="RESPONSE" shuffle="true" maxAssociations="0">
          <prompt>
          </prompt>
          <simpleAssociableChoice identifier="STEM_0" matchMax="1">1</simpleAssociableChoice>
          <simpleAssociableChoice identifier="OPTION_0" matchMax="1"><img alt="A row of two counters." src="650670.gif"/></simpleAssociableChoice>
          <simpleAssociableChoice identifier="OPTION_1" matchMax="1"><img alt="A row of three counters." src="650671.gif"/></simpleAssociableChoice>
          <simpleAssociableChoice identifier="OPTION_2" matchMax="1"><img alt="A row of five counters." src="650672.gif"/></simpleAssociableChoice>
        </associateInteraction>
      </itemBody>
      <responseProcessing template="http://www.imsglobal.org/question/qti_v2p1/rptemplates/map_response"/>
    </assessmentItem>
    val associateInteractionResult = AssociateInteractionTransformer.interactionJs(associateInteractionQti, QTIManifest.EmptyManifest)

    "transform prompt in Associate Interaction" in {
      val json = associateInteractionResult.values.headOption.getOrElse(throw new Exception("There was no result"))
      val prompt = (json \\ "prompt")(0).as[String]
      prompt must be equalTo (promptAI)
    }
  }
}