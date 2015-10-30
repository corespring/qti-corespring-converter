package org.corespring.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json.Json

class MatchInteractionTransformerSpec extends Specification {

  val qti = <assessmentItem xmlns="http://www.imsglobal.org/xsd/imsqti_v2p1"
                            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                            xmlns:m="http://www.w3.org/1998/Math/MathML"
                            xsi:schemaLocation="http://www.imsglobal.org/xsd/imsqti_v2p1  http://www.imsglobal.org/xsd/qti/qtiv2p1/imsqti_v2p1p1.xsd"
                            identifier="item-128727" title="Sample Export - Reading Advanced - 128727" adaptive="false" timeDependent="false">
    <responseDeclaration identifier="RESPONSE" cardinality="multiple" baseType="directedPair">
      <correctResponse>
        <value>SB RA</value>
        <value>SA RB</value>
        <value>SD RC</value>
        <value>SC RD</value>
      </correctResponse>
      <mapping defaultValue="0">
        <mapEntry mapKey="SB RA" mappedValue="1"/>
        <mapEntry mapKey="SA RB" mappedValue="1"/>
        <mapEntry mapKey="SD RC" mappedValue="1"/>
        <mapEntry mapKey="SC RD" mappedValue="1"/>
      </mapping>
    </responseDeclaration>
    <outcomeDeclaration baseType="float" cardinality="single" identifier="SCORE"/>
    <stylesheet href="styles/export_package_reading_advanced.css" type="text/css"/>
    <itemBody>
      <div>
        <object type="text/html" data="passages/5578.html" />
      </div>
      <div>Matching Interaction for Passage 1</div>
      <matchInteraction responseIdentifier="RESPONSE" shuffle="false" maxAssociations="4">
        <prompt>COrner</prompt>
        <simpleMatchSet>
          <simpleAssociableChoice identifier="SA" matchMax="1" fixed="true">
            Antelope
          </simpleAssociableChoice>
          <simpleAssociableChoice identifier="SB" matchMax="1" fixed="true">
            Beaver
          </simpleAssociableChoice>
          <simpleAssociableChoice identifier="SC" matchMax="1" fixed="true">
            Chinchilla
          </simpleAssociableChoice>
          <simpleAssociableChoice identifier="SD" matchMax="1" fixed="true">
            Donkey
          </simpleAssociableChoice>
        </simpleMatchSet>
        <simpleMatchSet>
          <simpleAssociableChoice identifier="RA" matchMax="1" fixed="true">
            Beaver
          </simpleAssociableChoice>
          <simpleAssociableChoice identifier="RB" matchMax="1" fixed="true">
            Antelope
          </simpleAssociableChoice>
          <simpleAssociableChoice identifier="RC" matchMax="1" fixed="true">
            Donkey
          </simpleAssociableChoice>
          <simpleAssociableChoice identifier="RD" matchMax="1" fixed="true">
            Chinchilla
          </simpleAssociableChoice>
        </simpleMatchSet>
      </matchInteraction>
    </itemBody>
  </assessmentItem>

  "transfomr" should {

    "be awesome" in {
      println(Json.prettyPrint(MatchInteractionTransformer.interactionJs(qti, QTIManifest.EmptyManifest).head._2))
      true === true
    }

  }

}
