package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json.Reads._

class OrderInteractionTransformerHMHSpec extends Specification {

  "OrderInteractionTransformer" should {

    val promptOI =  s"Three of these statements describe possible outcomes of this demonstration.";

    var orderInteractionQti1 = <assessmentItem xmlns="http://www.imsglobal.org/xsd/imsqti_v2p1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.imsglobal.org/xsd/imsqti_v2p1  http://www.imsglobal.org/xsd/qti/qtiv2p1/imsqti_v2p1p2.xsd" identifier="LTNA20_G612_GSP_OA_110_QUESTION_659719591" title="Summarize Informational Texts: Oops! Q03" timeDependent="false" adaptive="false">
      <responseDeclaration identifier="RESPONSE" cardinality="ordered" baseType="identifier">
        <correctResponse>
          <value>CHOICE_3</value>
          <value>CHOICE_0</value>
          <value>CHOICE_4</value>
          <value>CHOICE_1</value>
          <value>CHOICE_2</value>
        </correctResponse>
      </responseDeclaration>
      <outcomeDeclaration identifier="SCORE" cardinality="single" baseType="float"/>
      <itemBody>
        <orderInteraction label="RESPONSE" responseIdentifier="RESPONSE" shuffle="false">
          <prompt>
            { promptOI }
          </prompt>
          <simpleChoice identifier="CHOICE_0">Making online corrections is simple, but telling readers about them is not.</simpleChoice>
          <simpleChoice identifier="CHOICE_1">Even when news outlets make corrections, people can share stories that are outdated or false.</simpleChoice>
          <simpleChoice identifier="CHOICE_2">To make sure news is correct, everyone should think about where information comes from and who cares whether it&#8217;s accurate.</simpleChoice>
          <simpleChoice identifier="CHOICE_3">When news outlets make an error, they try to correct it quickly, even if it&#8217;s minor.</simpleChoice>
          <simpleChoice identifier="CHOICE_4">Sometimes news outlets update a story rather than correct it, then let readers know about the changes.</simpleChoice>
        </orderInteraction>
      </itemBody>
      <responseProcessing template="http://www.imsglobal.org/question/qtiv2p1/rptemplates/match_correct.xml"/>
    </assessmentItem>
    val orderInteractionResult = OrderInteractionTransformer.interactionJs(orderInteractionQti1, QTIManifest.EmptyManifest)

    "transform prompt in Order Interaction" in {
      val json = orderInteractionResult.values.headOption.getOrElse(throw new Exception("There was no result"))
      val prompt = (json \\ "prompt")(0).as[String]
      prompt must be equalTo (promptOI)
    }

  }
}