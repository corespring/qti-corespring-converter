package org.corespring.conversion.qti.interactions

import org.corespring.conversion.qti.transformers._
import org.specs2.execute.Failure
import org.specs2.mutable.Specification
import play.api.libs.json._

import scala.xml._

class FeedbackBlockTransformerSpec extends Specification {

  val identifier = "Q_01"
  val anotherIdentifier = "Q_02"

  def feedbackIdentifier(identifier: String) = s"${identifier}_feedback"
  def feedbackIdentifier(identifier: String, outcomeIdentifier: String) = s"${identifier}_feedback_${outcomeIdentifier}"

  def qti(correctResponses: Seq[String], correctFeedback: String, incorrectFeedback: String): Node =
    <assessmentItem>
      <responseDeclaration identifier={ identifier } cardinality="single" baseType="string">
        <correctResponse>
          { correctResponses.map(response => <value>{ response }</value>) }
        </correctResponse>
      </responseDeclaration>
      <itemBody>
        <p>This is some info that's in the prompt</p>
        {
        Seq(identifier, anotherIdentifier).map(id => {
          Seq(
              <focusTaskInteraction responseIdentifier={ id } expectedLength="15"/>,
            correctResponses.map(response =>
              <feedbackBlock outcomeIdentifier={ s"responses.$id.value" } identifier={ response }>
                <div class="feedback-block-correct">{ s"$correctFeedback $id" }</div>
              </feedbackBlock>),
            <feedbackBlock outcomeIdentifier={ s"responses.$id.value" } incorrectResponse="true">
              <div class="feedback-block-incorrect">{ s"$incorrectFeedback $id" }</div>
            </feedbackBlock>)
        })
        }
      </itemBody>
    </assessmentItem>

  def outcomeSpecificQti(correctResponses: Seq[String], correctFeedback: String, incorrectFeedback: String): Node =
    <assessmentItem>
      <responseDeclaration identifier={ identifier } cardinality="single" baseType="string">
        <correctResponse>
          { correctResponses.map(response => <value>{ response }</value>) }
        </correctResponse>
      </responseDeclaration>
      <itemBody>
        <p>This is some info that's in the prompt</p>
        <textEntryInteraction responseIdentifier={ identifier } expectedLength="15"/>
        {
        correctResponses.map(response =>
          <feedbackBlock outcomeIdentifier={ s"responses.$identifier.outcome.$response" } identifier={ response } incorrectResponse="false">
            <div class="feedback-block-correct">{ correctFeedback }</div>
          </feedbackBlock>)
        }
      </itemBody>
    </assessmentItem>

  "FeedbackBlockTransformer" should {

    val correctResponses = Seq("a", "b", "c")
    val correctFeedback = "That's correct!"
    val incorrectFeedback = "Oops! Not right."

    val input = qti(
      correctResponses = correctResponses,
      correctFeedback = correctFeedback,
      incorrectFeedback = incorrectFeedback)

    val outcomeSpecificInput = outcomeSpecificQti(
      correctResponses = correctResponses,
      correctFeedback = correctFeedback,
      incorrectFeedback = incorrectFeedback)

    val componentsJson = FeedbackBlockTransformer.interactionJs(input, ItemTransformer.EmptyManifest) ++
      FeedbackBlockTransformer.interactionJs(outcomeSpecificInput, ItemTransformer.EmptyManifest)
    val output = new InteractionRuleTransformer(FeedbackBlockTransformer(input)).transform(input)
    val outcomeSpecificOutput =
      new InteractionRuleTransformer(FeedbackBlockTransformer(outcomeSpecificInput)).transform(outcomeSpecificInput)

    def feedbackResult(identifier: String, outcomeIdentifier: String = null): JsObject = {
      Option(outcomeIdentifier) match {
        case Some(outcomeIdentifier) => componentsJson.get(feedbackIdentifier(identifier, outcomeIdentifier))
          .getOrElse(throw new RuntimeException(s"No outcome feedback component for $outcomeIdentifier and $identifier"))
        case None => componentsJson.get(feedbackIdentifier(identifier))
          .getOrElse(throw new RuntimeException(s"No feedback component for $identifier"))
      }
    }

    "return the correct feedback component type" in {
      (feedbackResult(identifier) \ "componentType").as[String] must be equalTo "corespring-feedback-block"
      correctResponses.map(correctResponse => {
        (feedbackResult(identifier, correctResponse) \ "componentType").as[String] must be equalTo "corespring-feedback-block"
      })
    }

    "return component weight as 0" in {
      (feedbackResult(identifier) \ "weight").as[Int] must be equalTo 0
    }

    def getFeedbackObjects(json: JsValue, key: String) =
      (json \ "feedback" \ key).as[JsArray].value

    def getFeedbackKeys(json: JsValue, key: String) =
      getFeedbackObjects(json, key).map((o: JsValue) => (o \ "input").as[String])

    def getFeedback(json: JsValue, key: String, response: String) =
      (getFeedbackObjects(json, key)
        .find((o: JsValue) => (o \ "input") == JsString(response))
        .get \ "feedback").as[String]

    "return correct feedback for answers" in {
      correctResponses.map(response => {
        getFeedbackKeys(feedbackResult(identifier), "correct").contains(response) must beTrue
      })
    }

    "return feedback text for answers with correct identifier" in {
      Seq(identifier, anotherIdentifier).map(id => {

        correctResponses.map(response => {
          getFeedback(feedbackResult(id), "correct", response) must be equalTo s"$correctFeedback $id"
        })
      }).flatten
    }

    "return correct outcome feedback" in {
      correctResponses.map(response => {
        val feedback = (feedbackResult(identifier, response) \ "feedback" \ "outcome" \ response)
        (outcomeSpecificInput \\ "feedbackBlock").find(f => (f \ "@outcomeIdentifier").text == s"responses.$identifier.outcome.$response") match {
          case Some(feedbackNode) => {
            (feedbackNode \ "@incorrectResponse").text match {
              case "true" => (feedback \ "correct").as[Boolean] must beFalse
              case _ => (feedback \ "correct").as[Boolean] must beTrue
            }
            feedbackNode.child.mkString must be equalTo (feedback \ "text").as[String]
          }
          case _ => Failure(s"Could not find feedback node for ${s"responses.$identifier.outcome.$response"}")
        }
      })
      success
    }

    "return incorrect feedback" in {
      Seq(identifier, anotherIdentifier).map(id => {
        getFeedbackKeys(feedbackResult(id), "incorrect").contains("*") must beTrue
        getFeedback(feedbackResult(id), "incorrect", "*") must be equalTo s"$incorrectFeedback $id"
      })
    }

    "replace all <feedbackBlock/>s with a <corespring-feedback-block/>" in {
      val finalOutput = FeedbackBlockTransformer.transform(output.head, ItemTransformer.EmptyManifest)
      (finalOutput \ "feedbackBlock").toSeq.length must be equalTo 0
      (finalOutput \\ "corespring-feedback-block").toSeq match {
        case seq if seq.isEmpty => failure("Output did not contain corespring-feedback-block")
        case seq => (seq.head \\ "@id").text must be equalTo feedbackIdentifier(identifier)
      }

      val finalOutcomeSpecificOutput =
        FeedbackBlockTransformer.transform(outcomeSpecificOutput.head, ItemTransformer.EmptyManifest)

      (finalOutcomeSpecificOutput \ "feedbackBlock").toSeq.length must be equalTo 0
      (finalOutcomeSpecificOutput \\ "corespring-feedback-block").toSeq match {
        case seq if seq.isEmpty => failure("Output did not contain corespring-feedback-block")
        case seq =>
          correctResponses.map(r => s"${identifier}_feedback_${r}").toSeq diff
            seq.map(n => (n \ "@id").text) must beEmpty
      }
    }

    "contain unique <corespring-feedback-block/> by id" in {
      Seq(identifier, anotherIdentifier).map(id => {
        (FeedbackBlockTransformer.transform(output.head, ItemTransformer.EmptyManifest) \\ "corespring-feedback-block")
          .filter(n => (n \ "@id").text == feedbackIdentifier(id)).toSeq.length must be equalTo 1
      })
    }

    "identifies correct feedback for <div class='feedback-block-correct'/>'" in {
      val response = "a"
      val feedback = "feedback<sup>html</sup>"

      def qti(response: String, feedback: String) = XML.loadString(s"""
        <assessmentItem>
          <responseDeclaration identifier="Q_01" cardinality="single" baseType="string">
            <correctResponse>
              <value>response</value>
            </correctResponse>
          </responseDeclaration>
          <itemBody>
            <focusTaskInteraction responseIdentifier="Q_01" expectedLength="15"></focusTaskInteraction>
            <feedbackBlock outcomeIdentifier="responses.Q_01.value" identifier="$response">
              <div class="feedback-block-correct">$feedback</div>
            </feedbackBlock>
          </itemBody>
        </assessmentItem>
        """)

      val input = qti(response, feedback)

      val json = new FeedbackBlockTransformer(input).interactionJs(input, ItemTransformer.EmptyManifest)
        .get("Q_01_feedback").getOrElse(throw new RuntimeException(s"No feedback component for Q_01"))

      getFeedback(json, "correct", response) === feedback
    }

  }

}