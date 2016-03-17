package org.parcconline.conversion.qti.processing

import org.corespring.conversion.qti.manifest.QTIManifest
import org.parcconline.conversion.qti.interactions.MatchInteractionTransformer
import org.specs2.mutable.Specification
import play.api.libs.json._

class PARCCMatchInteractionTransformerSpec extends Specification {

  val responseIdentifier = "RESPONSE"

  val categories = Map("t1" -> "Sentence 1", "t2" -> "Sentence 2", "t3" -> "Sentence 3")
  val choices = Map(
    "s1" -> "Katie's mother is disappointed that the girls use Japanese words incorrectly.",
    "s2" -> "Katie remembers when a dog ran out of a cornfield and attacked Lynn and her.",
    "s3" -> "Katie keeps Lynn's diary in a drawer beside her bed.",
    "s4" -> "Katie and Lynn spend much of their time together as they grow up.",
    "s5" -> "Lynn taught young Katie to say \"kira-kira,\"which was her first word.",
    "s6" -> "Katie believes that Lynn saves her life, but Lynn believes Katie saved her.",
    "s7" -> "Lynn explains that the sky is special like the ocean or people's eyes.",
    "s8" -> "Lynn sprayed the dog with water so it wouldn't hurt its tongue on broken glass."
  )

  val matchInteraction =
    <matchInteraction class="source-two-up target-one-up" maxAssociations="3" responseIdentifier={responseIdentifier}>
      <simpleMatchSet>
        {
          choices.map{ case(identifier, value) => {
            <simpleAssociableChoice class="whitebg text left" identifier={identifier} matchMax="1">{value}</simpleAssociableChoice>
          }}
        }
      </simpleMatchSet>
      <title>PARCC QTI Item Type Implementation <strong>Summary</strong></title>
      <simpleMatchSet>
        {
          categories.map{ case(identifier, label) =>
            <simpleAssociableChoice identifier={identifier} matchMax="1">{label}</simpleAssociableChoice>
          }
        }
      </simpleMatchSet>
    </matchInteraction>

  val correctResponse = Map("s4" -> "t1", "s2" -> "t2", "s6" -> "t3")

  val qti = <assessmentItem>
    <responseDeclaration baseType="directedPair" cardinality="multiple" identifier="RESPONSE">
      <correctResponse>
        {
          correctResponse.map{ case(key, value) => {
            <value>{s"$key $value"}</value>
          }}
        }
      </correctResponse>
    </responseDeclaration>
    <outcomeDeclaration baseType="float" cardinality="single" identifier="SCORE"/>
    <itemBody>
      <div class="row">
        <div class="span6">
          <p>
            <b>Today you will read two stories about characters whosave family members. As you read these stories, you will answer questions andthink about the characters. At the end of the task, you will be asked towrite an essay using the information from the stories.</b>
          </p>
          <div class="passage-scrolling passage440">
            <div class="stimulus_content">
              <p>
                <center>Permissions are pending for the passage,Kira Kira <i>by</i> Cynthia Kadohata.</center>
              </p>
            </div>
          </div>
        </div>
        <div class="span6">
          <div class="stem">
            <p>
              Create a summary of the story using <b>three</b> ofthe sentences listed here. Drag the <b>three</b> sentences that describe keyideas from the story into the boxes titled Sentence 1, Sentence 2, andSentence 3. Place the sentences in the order they happened.
            </p>
          </div>
          {matchInteraction}
        </div>
      </div>
    </itemBody>
    <responseProcessing template="http://www.imsglobal.org/question/qti_v2p1/rptemplates/map_response"/>
  </assessmentItem>

  "transform" should {

    val result = MatchInteractionTransformer.transform(matchInteraction, QTIManifest.EmptyManifest).head

    "return a <corespring-dnd-categorize/> node with the correct identifier" in {
      result.label must be equalTo("corespring-dnd-categorize")
      (result \ "@id").text must be equalTo(responseIdentifier)
    }

  }

  "interactionJs" should {

    val result = MatchInteractionTransformer.interactionJs(qti, QTIManifest.EmptyManifest).get(responseIdentifier)
      .getOrElse(throw new Exception(s"Result did not contain interaction for $responseIdentifier"))

    "have componentType = 'corespring-dnd-categorize'" in {
      (result \ "componentType").as[String] must be equalTo("corespring-dnd-categorize")
    }

    "correctResponse" should {

      val correctResponseJs = (result \ "correctResponse").as[JsObject]

      "map <correctResponse/> values to keyed object" in {
        correctResponse.map{ case(key, value) => {
          (correctResponseJs \ key).as[Seq[String]] must be equalTo(Seq(value))
        }}.last
      }

    }

    "feedback" should {

      val feedback = (result \ "feedback").as[JsObject]

      "be 'none' for all types" in {
        feedback must be equalTo(Json.obj(
          "correctFeedbackType" -> "none",
          "partialFeedbackType" -> "none",
          "incorrectFeedbackType" -> "none"
        ))
      }

    }

    "allowPartialScoring must be set to false" in {
      (result \ "allowPartialScoring").as[Boolean] must beFalse
    }

    "model" should {

      val model = (result \ "model").as[JsObject]

      "categories" should {

        val categoriesJs = (model \ "categories").as[Seq[JsObject]]

        "have second <simpleMatchSet>s <simpleAssociableChoice>s with id set to identifier, label set to content" in {
          categories.map{ case(identifier, label) =>
            categoriesJs must contain(Json.obj("id" -> identifier, "label" -> label))
          }.last
        }

      }

      "choices" should {

        val choicesJs = (model \ "choices").as[Seq[JsObject]]

        "have first <simpleMatchSet>s <simpleAssociableChoice>s with id set to identifier, label set to content" in {
          choicesJs.map{ case json => choices.toSeq must contain((json \ "id").as[String] -> (json \ "label").as[String])}.last
        }

        "have 'moveOnDrag' set to true" in {
          choicesJs.map{ choice =>
            (choice \ "moveOnDrag").as[Boolean] must beTrue
          }
        }

      }

      "config" should {

        val config = (model \ "config").as[JsObject]

        "have 'shuffle' = false" in {
          (config \ "shuffle").as[Boolean] must beFalse
        }

        "have answerAreaPosition = 'above'" in {
          (config \ "answerAreaPosition").as[String] must be equalTo("above")
        }

        "have categoriesPerRow = 1" in {
          (config \ "categoriesPerRow").as[Int] must be equalTo(1)
        }

        "have choicesPerRow = 2" in {
          (config \ "choicesPerRow").as[Int] must be equalTo(2)
        }

        "have an empty choicesLabel" in {
          (config \ "choicesLabel").as[String] must be equalTo("")
        }

        "have removeAllAfterPlacing = true" in {
          (config \ "removeAllAfterPlacing").as[Boolean] must beTrue
        }

      }

    }

  }


}
