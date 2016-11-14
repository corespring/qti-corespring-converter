package com.keydatasys.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json.Json

class KDSMatchInteractionTransformerSpec extends Specification {

  val qti = <assessmentItem xmlns="http://www.imsglobal.org/xsd/imsqti_v2p1" xmlns:m="http://www.w3.org/1998/Math/MathML" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.imsglobal.org/xsd/imsqti_v2p1 http://www.imsglobal.org/xsd/imsqti_v2p1.xsd" title="668136" identifier="668136" adaptive="false" timeDependent="false">
    <responseDeclaration identifier="RESPONSE1" cardinality="multiple" baseType="directedPair">
      <correctResponse>
        <value>Row1 Col2</value>
        <value>Row3 Col1</value>
      </correctResponse>
    </responseDeclaration>
    <outcomeDeclaration identifier="SCORE" cardinality="single" baseType="float">
      <defaultValue>
        <value>0</value>
      </defaultValue>
    </outcomeDeclaration>
    <stylesheet href="style/LiveInspect.css" type="text/css"/>
    <itemBody>
      <teacherInstructions>
        <![CDATA[<strong>TEACHER READS:</strong>
        <br />
        <br />Read and complete the task that follows.]]>
      </teacherInstructions>
      <matchInteraction responseIdentifier="RESPONSE1" shuffle="false" maxAssociations="2">
        <prompt visible="true">
          <![CDATA[<strong>The number of hours each student spent studying for a test in Classroom A and Classroom B are shown below.
            <br />
            <br />
            Classroom A:  1, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 8, 9, 11, 20
            <br />
            <br />
            Classroom B:  1, 4, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 8, 10, 22
            <br />
            <br />
            Determine whether each description of the summary statistic applies to the two data sets shown above. &nbsp;Select Yes or No for each summary statistic.</strong>]]>
        </prompt>
        <cornerText>
          <![CDATA[<strong>Summary Statistic</strong>]]>
        </cornerText>
        <simpleMatchSet>
          <simpleAssociableChoice identifier="Row1" matchMax="1">
            <![CDATA[The standard deviation and mean should be used to compare the data sets because the data sets do not contain any extreme outliers.]]>
          </simpleAssociableChoice>
          <simpleAssociableChoice identifier="Row2" matchMax="1">
            <![CDATA[The standard deviation and mean should be used to compare the data sets because the data sets are normally distributed.]]>
          </simpleAssociableChoice>
          <simpleAssociableChoice identifier="Row3" matchMax="1">
            <![CDATA[The interquartile range and median should be used to compare the data sets because both data sets contain extreme outliers and are skewed.]]>
          </simpleAssociableChoice>
        </simpleMatchSet>
        <simpleMatchSet>
          <simpleAssociableChoice identifier="Col1" matchMax="0">
            <![CDATA[<strong>Yes</strong>]]>
          </simpleAssociableChoice>
          <simpleAssociableChoice identifier="Col2" matchMax="0">
            <![CDATA[<strong>No</strong>]]>
          </simpleAssociableChoice>
        </simpleMatchSet>
      </matchInteraction>
    </itemBody>
    <responseProcessing template="http://www.imsglobal.org/question/qti_v2p1/rptemplates/match_correct"/>
  </assessmentItem>

  val json = MatchInteractionTransformer.interactionJs(qti, QTIManifest.EmptyManifest)

  "test" should {

    "be great" in {
      println(Json.prettyPrint(json.seq.head._2))
      true === true
    }

  }


}
