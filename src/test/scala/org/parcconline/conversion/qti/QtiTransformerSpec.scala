package org.parcconline.conversion.qti

import org.specs2.mutable.Specification
import play.api.libs.json.Json

class QtiTransformerSpec extends Specification {

  val item = <assessmentItem adaptive="false" identifier="M40072" timeDependent="false" title="Number">
    <responseDeclaration baseType="string" cardinality="single" identifier="RESPONSE1">
      <correctResponse>
        <value>1</value>
      </correctResponse>
    </responseDeclaration>
    <responseDeclaration baseType="string" cardinality="single" identifier="RESPONSE2">
      <correctResponse>
        <value>2</value>
      </correctResponse>
    </responseDeclaration>
    <responseDeclaration baseType="string" cardinality="single" identifier="RESPONSE3">
      <correctResponse>
        <value>-2</value>
      </correctResponse>
    </responseDeclaration>
    <responseDeclaration baseType="string" cardinality="single" identifier="RESPONSE4">
      <correctResponse>
        <value>5</value>
      </correctResponse>
    </responseDeclaration>
    <outcomeDeclaration baseType="float" cardinality="single" identifier="SCORE" normalMaximum="1" normalMinimum="0"/>
    <itemBody>
      <div class="row">
        <div class="span8">
          <div class="stem">
            <p id="p001">Let <span class="math_expression">
              <math xmlns="http://www.w3.org/1998/Math/MathML" alttext="f of x equals negative x plus three" display="inline" id="mml001">
                <mrow>
                  <mi>f</mi>
                  <mtext>(</mtext>
                  <mtext>&#x200A;</mtext>
                  <mi>x</mi>
                  <mtext>&#x200A;</mtext>
                  <mtext>)</mtext>
                  <mo>=</mo>
                  <mo>&#x2212;</mo>
                  <mtext>&#x200A;</mtext>
                  <mi>x</mi>
                  <mo>+</mo>
                  <mn>3</mn>
                </mrow>
              </math>
            </span> and <span class="math_expression">
              <math xmlns="http://www.w3.org/1998/Math/MathML" alttext="g of x equals three times the absolute value of x (pause) minus one" display="inline" id="mml002">
                <mrow>
                  <mi>g</mi>
                  <mtext>&#x200A;</mtext>
                  <mtext>(</mtext>
                  <mtext>&#x200A;</mtext>
                  <mi>x</mi>
                  <mtext>&#x200A;</mtext>
                  <mtext>)</mtext>
                  <mo>=</mo>
                  <mn>3</mn>
                  <mrow>
                    <mo>|</mo>
                    <mi>x</mi>
                    <mo>|</mo>
                  </mrow>
                  <mo>&#x2212;</mo>
                  <mn>1.</mn>
                </mrow>
              </math>
            </span> Where do the graphs of <span class="math_expression">
              <math xmlns="http://www.w3.org/1998/Math/MathML" alttext="f of x and g of x" display="inline" id="mml003">
                <mrow>
                  <mi>f</mi>
                  <mtext>(</mtext>
                  <mtext>&#x200A;</mtext>
                  <mi>x</mi>
                  <mtext>&#x200A;</mtext>
                  <mtext>)</mtext>
                </mrow>
              </math>
            </span> and
              <span class="math_expression">
                <math xmlns="http://www.w3.org/1998/Math/MathML" alttext="g of x" display="inline" id="mml004">
                  <mrow>
                    <mi>g</mi>
                    <mtext>(</mtext>
                    <mtext>&#x200A;</mtext>
                    <mi>x</mi>
                    <mtext>&#x200A;</mtext>
                    <mtext>)</mtext>
                  </mrow>
                </math>
              </span> intersect?</p>
            <p id="p002">Enter your answer in the boxes.</p>
            <p id="p003"/>
          </div>
          <div class="interaction">
            <p id="p004">(<textEntryInteraction expectedLength="4" id="int001" patternMask="[0-9\.\-]" responseIdentifier="RESPONSE1"/>, <textEntryInteraction expectedLength="4" id="int002" patternMask="[0-9\.\-]" responseIdentifier="RESPONSE2"/>) (<textEntryInteraction expectedLength="4" id="int003" patternMask="[0-9\.\-]" responseIdentifier="RESPONSE3"/>, <textEntryInteraction expectedLength="4" id="int004" patternMask="[0-9\.\-]" responseIdentifier="RESPONSE4"/>)</p>
          </div>
        </div>
      </div>
    </itemBody>
    <responseProcessing>
      <responseCondition>
        <responseIf>
          <or>
            <and>
              <equal toleranceMode="exact">
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <variable identifier="RESPONSE1"/>
                </customOperator>
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <correct identifier="RESPONSE1"/>
                </customOperator>
              </equal>
              <equal toleranceMode="exact">
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <variable identifier="RESPONSE2"/>
                </customOperator>
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <correct identifier="RESPONSE2"/>
                </customOperator>
              </equal>
              <equal toleranceMode="exact">
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <variable identifier="RESPONSE3"/>
                </customOperator>
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <correct identifier="RESPONSE3"/>
                </customOperator>
              </equal>
              <equal toleranceMode="exact">
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <variable identifier="RESPONSE4"/>
                </customOperator>
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <correct identifier="RESPONSE4"/>
                </customOperator>
              </equal>
            </and>
            <and>
              <equal toleranceMode="exact">
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <variable identifier="RESPONSE1"/>
                </customOperator>
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <correct identifier="RESPONSE3"/>
                </customOperator>
              </equal>
              <equal toleranceMode="exact">
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <variable identifier="RESPONSE2"/>
                </customOperator>
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <correct identifier="RESPONSE4"/>
                </customOperator>
              </equal>
              <equal toleranceMode="exact">
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <variable identifier="RESPONSE3"/>
                </customOperator>
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <correct identifier="RESPONSE1"/>
                </customOperator>
              </equal>
              <equal toleranceMode="exact">
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <variable identifier="RESPONSE4"/>
                </customOperator>
                <customOperator class="qti.customOperators.text.StringToNumber">
                  <correct identifier="RESPONSE2"/>
                </customOperator>
              </equal>
            </and>
          </or>
          <setOutcomeValue identifier="SCORE">
            <baseValue baseType="float">1</baseValue>
          </setOutcomeValue>
        </responseIf>
        <responseElse>
          <setOutcomeValue identifier="SCORE">
            <baseValue baseType="float">0</baseValue>
          </setOutcomeValue>
        </responseElse>
      </responseCondition>
    </responseProcessing>
    <apipAccessibility xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/imsapip_qtiv1p0">
      <inclusionOrder>
        <textOnlyDefaultOrder>
          <elementOrder identifierRef="ae001">
            <order>1</order>
          </elementOrder>
          <elementOrder identifierRef="ae002">
            <order>2</order>
          </elementOrder>
          <elementOrder identifierRef="ae003">
            <order>3</order>
          </elementOrder>
          <elementOrder identifierRef="ae004">
            <order>4</order>
          </elementOrder>
          <elementOrder identifierRef="ae005">
            <order>5</order>
          </elementOrder>
          <elementOrder identifierRef="ae006">
            <order>6</order>
          </elementOrder>
          <elementOrder identifierRef="ae007">
            <order>7</order>
          </elementOrder>
          <elementOrder identifierRef="ae008">
            <order>8</order>
          </elementOrder>
          <elementOrder identifierRef="ae009">
            <order>9</order>
          </elementOrder>
          <elementOrder identifierRef="ae010">
            <order>10</order>
          </elementOrder>
          <elementOrder identifierRef="ae011">
            <order>11</order>
          </elementOrder>
          <elementOrder identifierRef="ae012">
            <order>12</order>
          </elementOrder>
        </textOnlyDefaultOrder>
        <textGraphicsDefaultOrder>
          <elementOrder identifierRef="ae001">
            <order>0</order>
          </elementOrder>
          <elementOrder identifierRef="ae006">
            <order>1</order>
          </elementOrder>
          <elementOrder identifierRef="ae008">
            <order>2</order>
          </elementOrder>
        </textGraphicsDefaultOrder>
        <nonVisualDefaultOrder>
          <elementOrder identifierRef="ae001">
            <order>1</order>
          </elementOrder>
          <elementOrder identifierRef="ae002">
            <order>2</order>
          </elementOrder>
          <elementOrder identifierRef="ae003">
            <order>3</order>
          </elementOrder>
          <elementOrder identifierRef="ae004">
            <order>4</order>
          </elementOrder>
          <elementOrder identifierRef="ae005">
            <order>5</order>
          </elementOrder>
          <elementOrder identifierRef="ae006">
            <order>6</order>
          </elementOrder>
          <elementOrder identifierRef="ae007">
            <order>7</order>
          </elementOrder>
          <elementOrder identifierRef="ae008">
            <order>8</order>
          </elementOrder>
          <elementOrder identifierRef="ae009">
            <order>9</order>
          </elementOrder>
          <elementOrder identifierRef="ae010">
            <order>10</order>
          </elementOrder>
          <elementOrder identifierRef="ae011">
            <order>11</order>
          </elementOrder>
          <elementOrder identifierRef="ae012">
            <order>12</order>
          </elementOrder>
        </nonVisualDefaultOrder>
        <brailleDefaultOrder>
          <elementOrder identifierRef="ae001">
            <order>1</order>
          </elementOrder>
          <elementOrder identifierRef="ae002">
            <order>2</order>
          </elementOrder>
          <elementOrder identifierRef="ae003">
            <order>3</order>
          </elementOrder>
          <elementOrder identifierRef="ae004">
            <order>4</order>
          </elementOrder>
          <elementOrder identifierRef="ae005">
            <order>5</order>
          </elementOrder>
          <elementOrder identifierRef="ae006">
            <order>6</order>
          </elementOrder>
          <elementOrder identifierRef="ae007">
            <order>7</order>
          </elementOrder>
          <elementOrder identifierRef="ae008">
            <order>8</order>
          </elementOrder>
          <elementOrder identifierRef="ae009">
            <order>9</order>
          </elementOrder>
          <elementOrder identifierRef="ae010">
            <order>10</order>
          </elementOrder>
          <elementOrder identifierRef="ae011">
            <order>11</order>
          </elementOrder>
          <elementOrder identifierRef="ae012">
            <order>12</order>
          </elementOrder>
        </brailleDefaultOrder>
      </inclusionOrder>
      <accessibilityInfo>
        <accessElement identifier="ae001" xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/imsapip_qtiv1p0">
          <contentLinkInfo qtiLinkIdentifierRef="p001">
            <textLink>
              <fullString/>
            </textLink>
          </contentLinkInfo>
          <relatedElementInfo>
            <brailleText>
              <brailleTextString contentLinkIdentifier="br001">Let f of x equals negative x plus three and g of x equals three times the absolute value of x (pause) minus one Where do the graphs of f of x and g of x and
                g of x intersect?</brailleTextString>
            </brailleText>
            <spoken>
              <spokenText contentLinkIdentifier="st001">Let, f of x, equals, negative x, plus three, and, g of x, equals, three times the absolute value of x, minus one. Where do the graphs of f of x and g of x intersect.</spokenText>
              <textToSpeechPronunciation contentLinkIdentifier="ttsp001">Let, f of x, equal, negative x, plus three; and, g of x, equal, three, the absolute value of x, minus one. Where do the graphs of, f of x, and, g of x, intersect.</textToSpeechPronunciation>
            </spoken>
          </relatedElementInfo>
        </accessElement>
        <accessElement identifier="ae002" xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/imsapip_qtiv1p0">
          <contentLinkInfo qtiLinkIdentifierRef="mml001">
            <textLink>
              <fullString/>
            </textLink>
          </contentLinkInfo>
          <relatedElementInfo>
            <spoken>
              <spokenText contentLinkIdentifier="st002">f of x equals negative x plus three</spokenText>
              <textToSpeechPronunciation contentLinkIdentifier="ttsp002">f of x equals negative x plus three</textToSpeechPronunciation>
            </spoken>
            <brailleText>
              <brailleTextString contentLinkIdentifier="br002">f of x equals negative x plus three</brailleTextString>
            </brailleText>
          </relatedElementInfo>
        </accessElement>
        <accessElement identifier="ae003" xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/imsapip_qtiv1p0">
          <contentLinkInfo qtiLinkIdentifierRef="mml002">
            <textLink>
              <fullString/>
            </textLink>
          </contentLinkInfo>
          <relatedElementInfo>
            <spoken>
              <spokenText contentLinkIdentifier="st003">g of x equals three times the absolute value of x (pause) minus one</spokenText>
              <textToSpeechPronunciation contentLinkIdentifier="ttsp003">g of x equals three times the absolute value of x (pause) minus one</textToSpeechPronunciation>
            </spoken>
            <brailleText>
              <brailleTextString contentLinkIdentifier="br003">g of x equals three times the absolute value of x (pause) minus one</brailleTextString>
            </brailleText>
          </relatedElementInfo>
        </accessElement>
        <accessElement identifier="ae004" xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/imsapip_qtiv1p0">
          <contentLinkInfo qtiLinkIdentifierRef="mml003">
            <textLink>
              <fullString/>
            </textLink>
          </contentLinkInfo>
          <relatedElementInfo>
            <spoken>
              <spokenText contentLinkIdentifier="st004">f of x and g of x</spokenText>
              <textToSpeechPronunciation contentLinkIdentifier="ttsp004">f of x and g of x</textToSpeechPronunciation>
            </spoken>
            <brailleText>
              <brailleTextString contentLinkIdentifier="br004">f of x and g of x</brailleTextString>
            </brailleText>
          </relatedElementInfo>
        </accessElement>
        <accessElement identifier="ae005" xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/imsapip_qtiv1p0">
          <contentLinkInfo qtiLinkIdentifierRef="mml004">
            <textLink>
              <fullString/>
            </textLink>
          </contentLinkInfo>
          <relatedElementInfo>
            <spoken>
              <spokenText contentLinkIdentifier="st005">g of x</spokenText>
              <textToSpeechPronunciation contentLinkIdentifier="ttsp005">g of x</textToSpeechPronunciation>
            </spoken>
            <brailleText>
              <brailleTextString contentLinkIdentifier="br005">g of x</brailleTextString>
            </brailleText>
          </relatedElementInfo>
        </accessElement>
        <accessElement identifier="ae006" xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/imsapip_qtiv1p0">
          <contentLinkInfo qtiLinkIdentifierRef="p002">
            <textLink>
              <fullString/>
            </textLink>
          </contentLinkInfo>
          <relatedElementInfo>
            <brailleText>
              <brailleTextString contentLinkIdentifier="br006">Enter your answer in the boxes.</brailleTextString>
            </brailleText>
            <spoken>
              <spokenText contentLinkIdentifier="st006">Enter your answer in the boxes.</spokenText>
              <textToSpeechPronunciation contentLinkIdentifier="ttsp006">Enter your answer, in the boxes.</textToSpeechPronunciation>
            </spoken>
          </relatedElementInfo>
        </accessElement>
        <accessElement identifier="ae007" xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/imsapip_qtiv1p0">
          <contentLinkInfo qtiLinkIdentifierRef="p003">
            <textLink>
              <fullString/>
            </textLink>
          </contentLinkInfo>
          <relatedElementInfo>
            <spoken>
              <spokenText contentLinkIdentifier="st007">Object with no description.</spokenText>
              <textToSpeechPronunciation contentLinkIdentifier="ttsp007">Object with no description.</textToSpeechPronunciation>
            </spoken>
            <brailleText>
              <brailleTextString contentLinkIdentifier="br007">Object with no description.</brailleTextString>
            </brailleText>
          </relatedElementInfo>
        </accessElement>
        <accessElement identifier="ae008" xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/imsapip_qtiv1p0">
          <contentLinkInfo qtiLinkIdentifierRef="p004">
            <textLink>
              <fullString/>
            </textLink>
          </contentLinkInfo>
          <relatedElementInfo>
            <brailleText>
              <brailleTextString contentLinkIdentifier="br008">(Object with no description., Object with no description.) (Object with no description., Object with no description.)</brailleTextString>
            </brailleText>
            <spoken>
              <spokenText contentLinkIdentifier="st008">(Object with no description., Object with no description.) (Object with no description., Object with no description.)</spokenText>
              <textToSpeechPronunciation contentLinkIdentifier="ttsp008">Ordered pair, blank, blank. Ordered pair, blank, blank.</textToSpeechPronunciation>
            </spoken>
          </relatedElementInfo>
        </accessElement>
        <accessElement identifier="ae009" xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/imsapip_qtiv1p0">
          <contentLinkInfo qtiLinkIdentifierRef="int001">
            <objectLink/>
          </contentLinkInfo>
          <relatedElementInfo>
            <spoken>
              <spokenText contentLinkIdentifier="st009">Object with no description.</spokenText>
              <textToSpeechPronunciation contentLinkIdentifier="ttsp009">Object with no description.</textToSpeechPronunciation>
            </spoken>
            <brailleText>
              <brailleTextString contentLinkIdentifier="br009">Object with no description.</brailleTextString>
            </brailleText>
          </relatedElementInfo>
        </accessElement>
        <accessElement identifier="ae010" xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/imsapip_qtiv1p0">
          <contentLinkInfo qtiLinkIdentifierRef="int002">
            <objectLink/>
          </contentLinkInfo>
          <relatedElementInfo>
            <spoken>
              <spokenText contentLinkIdentifier="st010">Object with no description.</spokenText>
              <textToSpeechPronunciation contentLinkIdentifier="ttsp010">Object with no description.</textToSpeechPronunciation>
            </spoken>
            <brailleText>
              <brailleTextString contentLinkIdentifier="br010">Object with no description.</brailleTextString>
            </brailleText>
          </relatedElementInfo>
        </accessElement>
        <accessElement identifier="ae011" xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/imsapip_qtiv1p0">
          <contentLinkInfo qtiLinkIdentifierRef="int003">
            <objectLink/>
          </contentLinkInfo>
          <relatedElementInfo>
            <spoken>
              <spokenText contentLinkIdentifier="st011">Object with no description.</spokenText>
              <textToSpeechPronunciation contentLinkIdentifier="ttsp011">Object with no description.</textToSpeechPronunciation>
            </spoken>
            <brailleText>
              <brailleTextString contentLinkIdentifier="br011">Object with no description.</brailleTextString>
            </brailleText>
          </relatedElementInfo>
        </accessElement>
        <accessElement identifier="ae012" xmlns="http://www.imsglobal.org/xsd/apip/apipv1p0/imsapip_qtiv1p0">
          <contentLinkInfo qtiLinkIdentifierRef="int004">
            <objectLink/>
          </contentLinkInfo>
          <relatedElementInfo>
            <spoken>
              <spokenText contentLinkIdentifier="st012">Object with no description.</spokenText>
              <textToSpeechPronunciation contentLinkIdentifier="ttsp012">Object with no description.</textToSpeechPronunciation>
            </spoken>
            <brailleText>
              <brailleTextString contentLinkIdentifier="br012">Object with no description.</brailleTextString>
            </brailleText>
          </relatedElementInfo>
        </accessElement>
      </accessibilityInfo>
    </apipAccessibility>
  </assessmentItem>
  "great" should {

    "things" in {
      println(Json.prettyPrint(new QtiTransformer().transform(item)))
      true === true
    }

  }

}