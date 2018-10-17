package com.keydatasys.conversion.qti

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

import scala.xml.Node

class KDSQtiTransformerTest extends Specification {



  "normalizeDenominator" should {

    trait normalizeScope extends Scope{

      def qti : Node
      def resource : Node

      lazy val denominator = {
        val t = new KDSQtiTransformer(KDSMode.PARCC)
        t.normalizeDenominator(resource, qti)
      }
    }

    "return none for empty nodes" in new normalizeScope {
      val qti = <assessmentItem></assessmentItem>
      val resource = <resource></resource>
      denominator  === None
    }

    "twoPoint" in new normalizeScope {
      val qti = <assessmentItem>
          <responseProcessing>
            <responseCondition>
              <responseIf>
                <or>
                  <match>
                    <variable identifier="RESPONSE11"/>
                    <baseValue baseType="string">-</baseValue>
                  </match>
                  <match>
                    <variable identifier="RESPONSE11"/>
                    <baseValue baseType="string">-</baseValue>
                  </match>
                </or>
                <setOutcomeValue identifier="NUMCORRECT">
                  <sum>
                    <variable identifier="NUMCORRECT"/>
                    <baseValue baseType="float">1</baseValue>
                  </sum>
                </setOutcomeValue>
              </responseIf>
            </responseCondition>
            <responseCondition>
              <responseIf>
                <match>
                  <variable identifier="RESPONSE2"/>
                  <correct identifier="RESPONSE2"/>
                </match>
                <setOutcomeValue identifier="NUMCORRECT">
                  <sum>
                    <variable identifier="NUMCORRECT"/>
                    <baseValue baseType="float">1</baseValue>
                  </sum>
                </setOutcomeValue>
              </responseIf>
            </responseCondition>
            <responseCondition>
              <responseIf>
                <or>
                  <match>
                    <variable identifier="RESPONSE31"/>
                    <baseValue baseType="string">0.75</baseValue>
                  </match>
                  <match>
                    <variable identifier="RESPONSE31"/>
                    <baseValue baseType="string">.75</baseValue>
                  </match>
                </or>
                <setOutcomeValue identifier="NUMCORRECT">
                  <sum>
                    <variable identifier="NUMCORRECT"/>
                    <baseValue baseType="float">1</baseValue>
                  </sum>
                </setOutcomeValue>
              </responseIf>
            </responseCondition>
            <responseCondition>
              <responseIf>
                <match>
                  <variable identifier="RESPONSE4"/>
                  <correct identifier="RESPONSE4"/>
                </match>
                <setOutcomeValue identifier="NUMCORRECT">
                  <sum>
                    <variable identifier="NUMCORRECT"/>
                    <baseValue baseType="float">1</baseValue>
                  </sum>
                </setOutcomeValue>
              </responseIf>
            </responseCondition>
            <responseCondition>
              <responseIf>
                <gt>
                  <variable identifier="NUMCORRECT"/>
                  <baseValue baseType="float">0</baseValue>
                </gt>
                <setOutcomeValue identifier="SCORE">
                  <variable identifier="NUMCORRECT"/>
                </setOutcomeValue>
              </responseIf>
              <responseElse>
                <setOutcomeValue identifier="SCORE">
                  <baseValue baseType="float">0</baseValue>
                </setOutcomeValue>
              </responseElse>
            </responseCondition>
          </responseProcessing>
        </assessmentItem>
      val resource = <resource>
          <metadata>
            <lom>
              <general>
                <itemTypeId>8</itemTypeId>
                <parccTwoPointScoring>1</parccTwoPointScoring>
              </general>
              <parts>
                <part itemTypeId="12"/>
                <part itemTypeId="4"/>
                <part itemTypeId="12"/>
                <part itemTypeId="3"/>
              </parts>
            </lom>
          </metadata>
        </resource>
      denominator  === Some(4)
    }
  }

}
