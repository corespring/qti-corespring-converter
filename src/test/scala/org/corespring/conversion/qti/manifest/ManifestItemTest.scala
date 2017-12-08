package org.corespring.conversion.qti.manifest

import org.specs2.mutable.Specification

class ManifestItemTest extends Specification {


  /**
    * Resource node sample:
    */
  val resource = <resource identifier="665182" type="imsqti_item_xmlv2p1" href="665182.xml">
    <metadata>
      <qtiMetadata>
        <timeDependent>false</timeDependent>
        <interactionType>multipleInteraction</interactionType>
        <feedbackType>none</feedbackType>
        <solutionAvailable>true</solutionAvailable>
      </qtiMetadata>
      <lom>
        <general>
          <identifier>665182</identifier>
          <parentId>665010</parentId>
          <classicId>665182</classicId>
          <classicParentId>665010</classicParentId>
          <passageId/>
          <classicPassageId/>
          <depthOfKnowledge>3</depthOfKnowledge>
          <bloomsLevel>3</bloomsLevel>
          <omit>false</omit>
          <replaceWith/>
          <allowCalculator>1</allowCalculator>
          <allowGrammarTools>0</allowGrammarTools>
          <displayFormatId/>
          <forceFullPage>1</forceFullPage>
          <itemTypeId>8</itemTypeId>
          <mediaTypeId>1</mediaTypeId>
          <consortiumId/>
          <useTwoPointScoring/>
          <sbacTwoPointScoring/>
          <parccTwoPointScoring>1</parccTwoPointScoring>
          <gradeLevel>06</gradeLevel>
          <subject>math</subject>
          <language>English</language>
          <languageEquivalentId/>
          <teacherRead>false</teacherRead>
          <studentRead>true</studentRead>
        </general>
        <claims>
          <claim claimNumber="1" subject="math" category="Priority Cluster" target="A" priority="1">
          </claim>
        </claims>
        <mathPractices>
          <mathPractice>2</mathPractice>
          <mathPractice>4</mathPractice>
          <mathPractice>5</mathPractice>
          <mathPractice>7</mathPractice>
          <mathPractice>8</mathPractice>
        </mathPractices>
        <mathTools>
          <calculator>basic</calculator>
        </mathTools>
        <parts>
          <part itemTypeId="19"/>
          <part itemTypeId="12"/>
          <part itemTypeId="10"/>
          <part itemTypeId="12"/>
        </parts>
        <lifecycle>
          <version>2.1</version>
          <status>
            <source>LOMv1.0</source>
            <value>Final</value>
          </status>
        </lifecycle>
        <technical>
          <format>text/x-imsqti-item-xml</format>
          <format>image/gif</format>
        </technical>
      </lom>
    </metadata>
    <file href="665182.xml"/>
    <file href="images/665182_p03.gif"/>
    <file href="images/665182_p03_gi01.gif"/>
    <file href="images/665182_p03_gi02.gif"/>
    <file href="images/665182_p03_gi03.gif"/>
    <file href="images/665182_p03_gi04.gif"/>
    <file href="style/LiveInspect.css"/>
  </resource>
}
