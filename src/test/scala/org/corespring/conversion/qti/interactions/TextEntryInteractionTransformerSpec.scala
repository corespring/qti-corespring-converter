package org.corespring.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.equation.DomainParser
import org.corespring.conversion.qti.manifest.QTIManifest
import org.corespring.conversion.qti.transformers._
import org.specs2.mutable.Specification
import play.api.libs.json._

import scala.xml._

class TextEntryInteractionTransformerTest extends Specification with DomainParser {

  val identifier = "Q_01"
  val equationIdentifier = "Q_02"
  val lineIdentifier = "Q_03"

  def qti(correctResponses: Seq[String], correctFeedback: String, incorrectFeedback: String, popupFeedback: Boolean): Node =
    XML.loadString(s"""
    <assessmentItem>
      <responseDeclaration identifier="${identifier}" cardinality="single" baseType="string">
        <correctResponse>
          ${correctResponses.map(response => s"<value>${response}</value>")}
        </correctResponse>
      </responseDeclaration>
      <itemBody>
        <p>This is some info that's in the prompt</p>
        <textEntryInteraction responseIdentifier="${identifier}" expectedLength="15" popupFeedback="${popupFeedback}" />
        <feedbackBlock outcomeIdentifier="responses.${identifier}.value" identifier="someCorrect"><div>correct</div></feedbackBlock>
        <feedbackBlock outcomeIdentifier="responses.${identifier}.value" incorrectResponse="true" ><div>incorrect</div></feedbackBlock>
        <feedbackBlock outcomeIdentifier="responses.someOther.value" incorrectResponse="true" ><div>incorrect</div></feedbackBlock>
      </itemBody>
    </assessmentItem>
    """)

  def equationQti(equation: String, vars: String, domain: String, sigfigs: Int, popupFeedback: Boolean): Node = {
    val baseType = s"eqn: vars:$vars domain:$domain sigfigs:$sigfigs"
    <assessmentItem>
      <responseDeclaration identifier={ equationIdentifier } cardinality="single" baseType={ baseType }>
        <correctResponse>
          <value>{ equation }</value>
        </correctResponse>
      </responseDeclaration>
      <itemBody>
        <p>This is some info that's in the prompt</p>
        <textEntryInteraction responseIdentifier={ equationIdentifier } expectedLength="15" popupFeedback={ popupFeedback.toString }/>
      </itemBody>
    </assessmentItem>
  }

  def lineQti(equation: String): Node = {
    <assessmentItem>
      <responseDeclaration identifier={ lineIdentifier } cardinality="single" baseType="line">
        <correctResponse>
          <value>{ equation }</value>
        </correctResponse>
      </responseDeclaration>
      <itemBody>
        <p>This is some info that's in the prompt</p>
        <textEntryInteraction responseIdentifier={ lineIdentifier } expectedLength="15"/>
      </itemBody>
    </assessmentItem>
  }

  "TextEntryInteractionTransformer" should {

    val correctResponses = Seq("a", "b", "c")
    val correctFeedback = "That's correct!"
    val incorrectFeedback = "Oops! Not right."

    val input = qti(
      correctResponses = correctResponses,
      correctFeedback = correctFeedback,
      incorrectFeedback = incorrectFeedback,
      popupFeedback = true)

    val inputNoPopup = qti(
      correctResponses = correctResponses,
      correctFeedback = correctFeedback,
      incorrectFeedback = incorrectFeedback,
      popupFeedback = false)

    val interactionResult = TextEntryInteractionTransformer(input).interactionJs(input, QTIManifest.EmptyManifest)
      .get(identifier).getOrElse(throw new RuntimeException(s"No component called $identifier"))

    val output = new InteractionRuleTransformer(TextEntryInteractionTransformer(input)).transform(input)
    val outputNoPopup = new InteractionRuleTransformer(TextEntryInteractionTransformer(inputNoPopup)).transform(inputNoPopup)

    val equation = "y=2x+7"
    val vars = "x,y"
    val domain = "-10->10,0"
    val sigfigs = 3

    val equationInput = equationQti(equation, vars, domain, sigfigs, true)
    val lineInput = lineQti(equation)

    val lineOutput = new InteractionRuleTransformer(new TextEntryInteractionTransformer(lineInput)).transform(lineInput)

    val equationInteractionResult =
      new TextEntryInteractionTransformer(equationInput)
        .interactionJs(equationInput, QTIManifest.EmptyManifest).get(equationIdentifier)
        .getOrElse(throw new RuntimeException(s"No component called $equationIdentifier"))

    new TextEntryInteractionTransformer(lineInput).interactionJs(lineInput, QTIManifest.EmptyManifest)
      .get(lineIdentifier).getOrElse(throw new RuntimeException(s"No component called $lineIdentifier"))

    "return the correct interaction component type" in {
      (interactionResult \ "componentType").as[String] must be equalTo "corespring-text-entry"
      (equationInteractionResult \ "componentType").as[String] must be equalTo "corespring-function-entry"
    }

    "return the correct answers for the interaction" in {
      val ss = (interactionResult \ "correctResponses" \ "values").as[Seq[String]]
      ss.diff(correctResponses) === Seq.empty
    }

    "returns the correct correct response vars" in {
      (equationInteractionResult \ "correctResponse" \ "vars").as[String] must be equalTo vars
    }

    "returns the correct correct response domain" in {
      (equationInteractionResult \ "correctResponse" \ "domain") === parseDomain(domain)
    }

    "returns the correct correct response sigfigs" in {
      (equationInteractionResult \ "correctResponse" \ "sigfigs").as[Int] must be equalTo sigfigs
    }

    "returns the correct correct response equation" in {
      (equationInteractionResult \ "correctResponse" \ "equation").as[String] must be equalTo equation
    }

    "returns feedback type default for correct answers" in {
      (equationInteractionResult \ "feedback" \ "correctFeedbackType").as[String] must be equalTo "default"
    }

    "returns feedback type default for correct answers" in {
      (equationInteractionResult \ "feedback" \ "incorrectFeedbackType").as[String] must be equalTo "default"
    }

    "text entry feedback blocks are removed from the xml (popup)" in {
      // only feedback blocks that do not belong to text entry interactions are left in the xml
      (output \\ "feedbackBlock").size == 1
      !(output \\ "feedbackBlock").find(n => (n \ "@outcomeIdentifier").text == "responses.someOther.value").isEmpty
    }

    "text entry feedback blocks are not removed from the xml (no popup)" in {
      // only feedback blocks that do not belong to text entry interactions are left in the xml
      (outputNoPopup \\ "feedbackBlock").size == 3
    }

    "correct feedback is extracted from feedback blocks" in {
      (interactionResult \ "correctResponses" \ "feedback" \ "specific") === Json.arr(
        Json.obj(
          "answer" -> "someCorrect",
          "feedback" -> "<div>correct</div>"))
    }

    "incorrect feedback is extracted from feedback blocks" in {
      (interactionResult \ "incorrectResponses" \ "feedback" \ "value") === JsString("<div>incorrect</div>")
    }

    "converts baseType=line to <corespring-function-entry/>" in {
      val s = (lineOutput \\ "corespring-function-entry")
      s !== Seq.empty
    }

  }

}
