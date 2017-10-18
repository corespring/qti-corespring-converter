package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.ChoiceInteractionBuilder
import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json.JsObject

import scala.xml.XML
import play.api.libs.json.Json._


class ChoiceInteractionTransformerTest extends Specification
  with ChoiceInteractionBuilder{

  val prompt = "ITEM <b>PROMPT</b>"
  val a = <img src="test.jpg"/>

  def choiceInteraction = XML.loadString(s"""

    <choiceInteraction responseIdentifier="Q_01" shuffle="false" maxChoices="1">
      <prompt>$prompt</prompt>
      <simpleChoice identifier="A">
        $a
        <feedbackInline identifier="A" defaultFeedback="true"/>
      </simpleChoice>
      <simpleChoice identifier="B">
        B
        <feedbackInline identifier="B" defaultFeedback="true"/>
      </simpleChoice>
    </choiceInteraction>
  """)


  "ChoiceInteractionTransformerTest" should {
    "interactionJs" in {

      val multipleChoice = qti(
        responseDeclaration("multiple", <correctResponse>
          <value>A</value><value>B</value>
        </correctResponse>),
        choiceInteraction)

     val out = ChoiceInteractionTransformer.interactionJs(multipleChoice, QTIManifest.EmptyManifest)

      val q1 = out.get("Q_01").get
      val choices = (q1 \ "model" \ "choices").as[Seq[JsObject]]
      val values = choices.map( c => (c \ "value").asOpt[String])
      values.flatten must_== Seq("A", "B")
      println(out)
     ok
    }

  }
}
