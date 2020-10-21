package org.corespring.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.corespring.conversion.qti.transformers.InteractionRuleTransformer
import org.specs2.mutable.Specification
import play.api.libs.json._

import scala.xml._

trait ChoiceInteractionBuilder {

  def qti(rd: Elem, body: Elem): Node =
    <assessmentItem>
      <correctResponseFeedback>Default Correct</correctResponseFeedback>
      <incorrectResponseFeedback>Default Incorrect</incorrectResponseFeedback>
      { rd }<itemBody>
      { body }
    </itemBody>
    </assessmentItem>

  def responseDeclaration(cardinality: String, correctResponse: Elem) =
    <responseDeclaration identifier="Q_01" cardinality={ cardinality } baseType="identifier">
      { correctResponse }
    </responseDeclaration>

}
class ChoiceInteractionTransformerTest extends Specification with ChoiceInteractionBuilder {

  val a = <img src="test.jpg"/>



  def prompt = "ITEM <b>PROMPT</b>"

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

  def choiceInterationWithHTML(html: NodeSeq) =
    <choiceInteraction responseIdentifier="Q_01" shuffle="false" maxChoices="1">
      { html }
      <simpleChoice identifier="A">Something</simpleChoice>
      <simpleChoice identifier="B">Something else</simpleChoice>
    </choiceInteraction>

  def inlineInteraction =
    <inlineChoiceInteraction responseIdentifier="Q_01" shuffle="false" maxChoices="1">
      <prompt>ITEM PROMPT?</prompt>
      <inlineChoice identifier="A">
        <math>A</math>
        <feedbackInline identifier="A" defaultFeedback="true"/>
      </inlineChoice>
      <inlineChoice identifier="B">
        <math>A</math>
        <feedbackInline identifier="B" defaultFeedback="true"/>
      </inlineChoice>
    </inlineChoiceInteraction>

  val singleChoice = qti(
    responseDeclaration("single", <correctResponse>
      <value>A</value>
    </correctResponse>),
    choiceInteraction)

  val inlineChoice = qti(
    responseDeclaration("single", <correctResponse>
      <value>A</value>
    </correctResponse>),
    inlineInteraction)

  val multipleChoice = qti(
    responseDeclaration("multiple", <correctResponse>
      <value>A</value><value>B</value>
    </correctResponse>),
    choiceInteraction)

  "ChoiceInteractionTransformer" should {

    "transform choiceInteraction" in {
      val out = new InteractionRuleTransformer(ChoiceInteractionTransformer).transform(singleChoice)
      val componentsJson = ChoiceInteractionTransformer.interactionJs(singleChoice, QTIManifest.EmptyManifest)
      val q1 = componentsJson.get("Q_01").getOrElse(throw new RuntimeException("No component called Q_01"))

      (out \\ "p").head.child.mkString === prompt
      (q1 \ "componentType").as[String] === "corespring-multiple-choice"
      (q1 \ "model" \ "config" \ "choiceType").as[String] === "radio"
      ((q1 \ "model" \ "choices")(0) \ "label").as[String] === a.toString
      (q1 \ "correctResponse" \ "value").as[Seq[String]] === Seq("A")
      (q1 \ "feedback").as[Seq[JsObject]].length === 2
      ((q1 \ "feedback")(0) \ "value").as[String] === "A"
      ((q1 \ "feedback")(0) \ "feedback").as[String] === "Default Correct"
    }

    "transform inlineChoiceInteraction" in {

      val out = new InteractionRuleTransformer(ChoiceInteractionTransformer).transform(inlineChoice)
      val q1 = ChoiceInteractionTransformer.interactionJs(inlineChoice, QTIManifest.EmptyManifest).get("Q_01")
        .getOrElse(throw new RuntimeException("No component called Q_01"))

      (q1 \ "componentType").as[String] === "corespring-inline-choice"
      ((q1 \ "model" \ "choices")(0) \ "label").as[String] === "<math>A</math>"
      (q1 \ "correctResponse").as[String] === "A"
      (q1 \ "feedback").as[Seq[JsObject]].length === 2
      ((q1 \ "feedback")(0) \ "value").as[String] === "A"
      ((q1 \ "feedback")(0) \ "feedbackType").as[String] === "default"
    }

    "preserve and move choiceInteraction HTML nodes outside interaction" in {
      val html = <div>This is some text</div><img src="and/an/image.png"/>;
      val out = new InteractionRuleTransformer(ChoiceInteractionTransformer).transform(choiceInterationWithHTML(html))
      out.containsSlice(html) must beTrue
    }
  }
}
