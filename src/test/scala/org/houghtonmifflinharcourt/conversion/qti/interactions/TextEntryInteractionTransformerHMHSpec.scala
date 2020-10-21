package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json.Reads._

class TextEntryInteractionTransformerHMHSpec extends Specification {

  "TextEntryInteractionTransformer" should {

    val responseIdentifier = "1"
    val correctResponse = "B"
    val choices = Map("A" -> "choice one", "B" -> "choice two","C" -> "choice three","D" -> "choice four")
    val promptTE =  s"Three of these statements describe possible outcomes of this demonstration.";
    val shuffle = false

    def textEntryQti(responseIdentifier: String = responseIdentifier,
                        correctResponse: String = correctResponse,
                        promptTE: String = promptTE,
                        choices: Map[String, String] = choices,
                        shuffle: Boolean = shuffle
                       ) =
      <assessmentItem xmlns="http://www.imsglobal.org/xsd/imsqti_v2p1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:m="http://www.w3.org/1998/Math/MathML" xsi:schemaLocation="http://www.imsglobal.org/xsd/imsqti_v2p1 http://www.imsglobal.org/xsd/qti/qtiv2p1/imsqti_v2p1p2.xsd http://www.w3.org/1998/Math/MathML http://www.w3.org/Math/XMLSchema/mathml2/mathml2.xsd" identifier="IMNL20E11_ayr_unstnd_addto_tkfrm_QUESTION_271610992_2" title="Module 05, Are You Ready Q02" timeDependent="false" adaptive="false">
        <responseDeclaration identifier="RESPONSEA" cardinality="single" baseType="string">
          <correctResponse>
            <value>5</value>
          </correctResponse>
        </responseDeclaration>
        <itemBody>
          <div>
            <p>How many dragonflies are left after you take away 2?</p>
            <p><strong>Part B</strong> Please ask your teacher for help to answer this question.<br/>Choose the correct number. Then choose the correct picture. You will use all of the numbers or pictures.</p>
          </div>
          <div>
            <blockquote>
              <p><strong><math xmlns='http://www.w3.org/1998/Math/MathML'><mn>7</mn> <mo>-</mo> <mn>2</mn> <mo>=</mo> </math></strong> <textEntryInteraction responseIdentifier="RESPONSEA" expectedLength="15"/></p>
            </blockquote>
          </div>
        </itemBody>
        <responseProcessing template="http://www.imsglobal.org/question/qtiv2p1/rptemplates/match_correct.xml"/>
      </assessmentItem>
    val textEntryInteractionResult = TextEntryInteractionTransformer.interactionJs(textEntryQti(), QTIManifest.EmptyManifest)

    "transform prompt in Text Entry Interaction" in {
      val json = textEntryInteractionResult.values.headOption.getOrElse(throw new Exception("There was no result"))
      val prompt = (json \\ "markup")(1).as[String]
      prompt.contains(promptTE)
    }
  }
}