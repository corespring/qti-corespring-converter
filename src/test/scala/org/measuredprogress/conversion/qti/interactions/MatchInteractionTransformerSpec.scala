package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json.Json

class MPMatchInteractionTransformerSpec extends Specification {

  "transformJS" should {

    val qti = <assessmentItem adaptive="false" identifier="ITEM-LOGIC-KS-995" timeDependent="false" title="In each row, click the box or boxes that describe the number." toolName="ItemLogic" toolVersion="2.0.0" xmlns="http://www.imsglobal.org/xsd/imsqti_v2p1" xmlns:m="http://www.w3.org/1998/Math/MathML" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.imsglobal.org/xsd/imsqti_v2p1  http://www.imsglobal.org/xsd/qti/qtiv2p1/imsqti_v2p1.xsd">
      <responseDeclaration baseType="directedPair" cardinality="multiple" identifier="RESPONSE1030">
        <correctResponse>
          <value>SAC-17309 SAC-17289</value>
          <value>SAC-17309 SAC-17293</value>
          <value>SAC-17311 SAC-17295</value>
          <value>SAC-17312 SAC-17291</value>
          <value>SAC-17312 SAC-17293</value>
        </correctResponse>
        <mapping defaultValue="0.00" lowerBound="0.00" upperBound="12.00">
          <mapEntry mapKey="SAC-17309 SAC-17289" mappedValue="0.00"/>
          <mapEntry mapKey="SAC-17309 SAC-17291" mappedValue="0.00"/>
          <mapEntry mapKey="SAC-17309 SAC-17293" mappedValue="0.00"/>
          <mapEntry mapKey="SAC-17309 SAC-17295" mappedValue="0.00"/>
          <mapEntry mapKey="SAC-17311 SAC-17289" mappedValue="0.00"/>
          <mapEntry mapKey="SAC-17311 SAC-17291" mappedValue="0.00"/>
          <mapEntry mapKey="SAC-17311 SAC-17293" mappedValue="0.00"/>
          <mapEntry mapKey="SAC-17311 SAC-17295" mappedValue="0.00"/>
          <mapEntry mapKey="SAC-17312 SAC-17289" mappedValue="0.00"/>
          <mapEntry mapKey="SAC-17312 SAC-17291" mappedValue="0.00"/>
          <mapEntry mapKey="SAC-17312 SAC-17293" mappedValue="0.00"/>
          <mapEntry mapKey="SAC-17312 SAC-17295" mappedValue="0.00"/>
        </mapping>
      </responseDeclaration>
      <responseDeclaration baseType="identifier" cardinality="single" identifier="RESPONSE1031">
        <correctResponse>
          <value>SC-18043</value>
        </correctResponse>
      </responseDeclaration>
      <outcomeDeclaration baseType="float" cardinality="single" identifier="SCORE1030">
        <defaultValue>
          <value>0</value>
        </defaultValue>
      </outcomeDeclaration>
      <outcomeDeclaration baseType="float" cardinality="single" identifier="SCORE1031">
        <defaultValue>
          <value>0</value>
        </defaultValue>
      </outcomeDeclaration>
      <outcomeDeclaration baseType="float" cardinality="single" identifier="SCORE">
        <defaultValue>
          <value>0</value>
        </defaultValue>
      </outcomeDeclaration>
      <stylesheet href="../styles/bundled.css" type="text/css"/>
      <stylesheet href="../styles/inline.css" type="text/css"/>
      <itemBody>
        <matchInteraction class="table" maxAssociations="12" responseIdentifier="RESPONSE1030" shuffle="false">
          <prompt>In each row, click the box or boxes that describe the number.<br/></prompt>
          <simpleMatchSet>
            <simpleAssociableChoice identifier="SAC-17309" matchMax="4">
              <p>306</p>
            </simpleAssociableChoice>
            <simpleAssociableChoice identifier="SAC-17311" matchMax="4">
              <p>630</p>
            </simpleAssociableChoice>
            <simpleAssociableChoice identifier="SAC-17312" matchMax="4">
              <p>360</p>
            </simpleAssociableChoice>
          </simpleMatchSet>
          <simpleMatchSet>
            <simpleAssociableChoice identifier="SAC-17289" matchMax="3">
              <p>has 6 ones</p>
            </simpleAssociableChoice>
            <simpleAssociableChoice identifier="SAC-17291" matchMax="3">
              <p>has 6 tens</p>
            </simpleAssociableChoice>
            <simpleAssociableChoice identifier="SAC-17293" matchMax="3">
              <p>has 3 hundreds</p>
            </simpleAssociableChoice>
            <simpleAssociableChoice identifier="SAC-17295" matchMax="3">
              <p>has 3 tens</p>
            </simpleAssociableChoice>
          </simpleMatchSet>
        </matchInteraction>
        <choiceInteraction maxChoices="1" responseIdentifier="RESPONSE1031" shuffle="false">
          <prompt>Which number has 10 tens and 10 ones?<br/></prompt>
          <simpleChoice fixed="true" identifier="SC-18045">
            <p>11</p>
          </simpleChoice>
          <simpleChoice fixed="true" identifier="SC-18046">
            <p>101</p>
          </simpleChoice>
          <simpleChoice fixed="true" identifier="SC-18043">
            <p>110</p>
          </simpleChoice>
          <simpleChoice fixed="true" identifier="SC-18044">
            <p>1010</p>
          </simpleChoice>
        </choiceInteraction>
      </itemBody>
      <responseProcessing>
        <responseCondition>
          <responseIf>
            <isNull>
              <variable identifier="RESPONSE1030"/>
            </isNull>
            <setOutcomeValue identifier="SCORE1030">
              <baseValue baseType="float">0.0</baseValue>
            </setOutcomeValue>
          </responseIf>
          <responseElse>
            <setOutcomeValue identifier="SCORE1030">
              <mapResponse identifier="RESPONSE1030"/>
            </setOutcomeValue>
          </responseElse>
        </responseCondition>
        <responseCondition>
          <responseIf>
            <match>
              <variable identifier="RESPONSE1031"/>
              <correct identifier="RESPONSE1031"/>
            </match>
            <setOutcomeValue identifier="SCORE1031">
              <baseValue baseType="float">1</baseValue>
            </setOutcomeValue>
          </responseIf>
          <responseElse>
            <setOutcomeValue identifier="SCORE1031">
              <baseValue baseType="float">0</baseValue>
            </setOutcomeValue>
          </responseElse>
        </responseCondition>
        <setOutcomeValue identifier="SCORE">
          <sum>
            <variable identifier="SCORE1030"/>
            <variable identifier="SCORE1031"/>
          </sum>
        </setOutcomeValue>
      </responseProcessing>
    </assessmentItem>

    "be amazeballs" in {
      println(Json.prettyPrint(MatchInteractionTransformer.interactionJs(qti, QTIManifest.EmptyManifest).head._2))
      true === true
    }

  }

}
