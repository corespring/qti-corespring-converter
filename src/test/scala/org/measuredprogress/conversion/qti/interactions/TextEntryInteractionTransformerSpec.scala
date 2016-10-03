package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json.Json

class MPTextEntryInteractionTransformerSpec extends Specification {

  val qti =
    <assessmentItem>
      <responseDeclaration baseType="string" cardinality="single" identifier="RESPONSE152">
        <correctResponse>
          <value>2</value>
        </correctResponse>
        <mapping>
          <mapEntry mapKey="2" mappedValue="1.00"/>
          <mapEntry mapKey="two" mappedValue="1.00"/>
          <mapEntry mapKey="2.0" mappedValue="1.00"/>
          <mapEntry mapKey="4/2" mappedValue="1.00"/>
          <mapEntry mapKey="2/1" mappedValue="1.00"/>
          <mapEntry mapKey="1/0.5" mappedValue="1.00"/>
        </mapping>
      </responseDeclaration>
      <responseDeclaration baseType="string" cardinality="single" identifier="RESPONSE153">
        <correctResponse>
          <value>1</value>
        </correctResponse>
        <mapping>
          <mapEntry mapKey="1" mappedValue="1.00"/>
          <mapEntry mapKey="one" mappedValue="1.00"/>
          <mapEntry mapKey="1.0" mappedValue="1.00"/>
          <mapEntry mapKey="1/1" mappedValue="1.00"/>
          <mapEntry mapKey="2/2" mappedValue="1.00"/>
          <mapEntry mapKey="0.5/0.5" mappedValue="1.00"/>
        </mapping>
      </responseDeclaration>
      <itemBody>
        <rubricBlock use="stimulus" view="candidate author proctor scorer testConstructor tutor">
          <div>
            <p>Kila and Ty made a table to show the distance and time they each walked. Ty walked at the same rate as Kila.</p>
          </div>
        </rubricBlock>
        <div class="stem stem_inline" xmlns:default="http://www.w3.org/1998/Math/MathML">
          <div class="stem_prompt">
            <p>Fill in the table with the rate that Kila walked and the distance Ty walked.</p>
          </div>
          <table class="style-a3f303f2f8a08e1dfaa5bdde04c0641d" xmlns:default="http://www.w3.org/1998/Math/MathML"> <tbody> <tr> <td class="style-25150780864b21cd93ec9cb82758edc1">&#xA0;</td> <td class="style-25150780864b21cd93ec9cb82758edc1"> <p><strong>Distance</strong></p> <p>(in miles)</p> </td> <td class="style-25150780864b21cd93ec9cb82758edc1"> <p><strong>Time</strong></p> <p>(in hours)</p> </td> <td class="style-25150780864b21cd93ec9cb82758edc1"> <p><strong>Rate</strong></p> <p>(miles per hour)</p> </td> </tr> <tr> <td class="style-25150780864b21cd93ec9cb82758edc1"><strong>Kila</strong></td> <td class="style-25150780864b21cd93ec9cb82758edc1">&#xA0; &#xA0; &#xA0;&#xA0; <m:math xmlns="http://www.w3.org/1998/Math/MathML">   <m:mfrac>     <m:mn>1</m:mn>     <m:mn>2</m:mn>   </m:mfrac> </m:math></td> <td class="style-25150780864b21cd93ec9cb82758edc1">&#xA0; &#xA0; &#xA0; <m:math xmlns="http://www.w3.org/1998/Math/MathML">   <m:mfrac>     <m:mn>1</m:mn>     <m:mn>4</m:mn>   </m:mfrac> </m:math></td> <td class="style-25150780864b21cd93ec9cb82758edc1">&#xA0;&#xA0; <textEntryInteraction responseIdentifier="RESPONSE152"/></td> </tr> <tr> <td class="style-25150780864b21cd93ec9cb82758edc1"><strong>Ty</strong></td> <td class="style-0594fd93e2a95d4b5ade904927f9cdf8"><textEntryInteraction responseIdentifier="RESPONSE153"/></td> <td class="style-25150780864b21cd93ec9cb82758edc1">&#xA0;&#xA0;&#xA0;&#xA0;&#xA0; <m:math xmlns="http://www.w3.org/1998/Math/MathML">   <m:mfrac>     <m:mn>1</m:mn>     <m:mn>2</m:mn>   </m:mfrac> </m:math></td> <td class="style-25150780864b21cd93ec9cb82758edc1">&#xA0;</td> </tr> </tbody> </table>
        </div>
      </itemBody>
    </assessmentItem>

  "interactionJs" should {

    "be cool" in {
      new TextEntryInteractionTransformer(qti).interactionJs(qti, QTIManifest.EmptyManifest).foreach{ case(id, json) => {
        println(Json.prettyPrint(json))
      }}
      true === true
    }

  }

}
