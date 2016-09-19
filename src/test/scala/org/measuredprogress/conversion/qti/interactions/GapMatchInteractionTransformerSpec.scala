package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.corespring.conversion.qti.transformers.InteractionRuleTransformer
import org.specs2.mutable.Specification
import play.api.libs.json.{JsObject, Json}

class GGapMatchInteractionTransformerSpec extends Specification {

  val identifier = "RESPONSE1059"

  val choices = Map(
    "GT-17980" -> "tip",
    "GT-17981" -> "back",
    "GT-17978" -> "brain",
    "GT-17982" -> "an olive",
    "GT-17977" -> "taste buds",
    "GT-17979" -> "a cupcake"
  )

  val correctResponse = Map(
    "GAP-17995" -> "GT-17977",
    "GAP-17996" -> "GT-17978",
    "GAP-17997" -> "GT-17979",
    "GAP-17998" -> "GT-17980"
  )

  val answerAreaIds = correctResponse.keys

  val qti =
    <assessmentItem>
      <responseDeclaration baseType="directedPair" cardinality="multiple" identifier={identifier}>
        <correctResponse>
          {
            correctResponse.map{ case(answerAreaId, choiceId) => {
              <value>{s"$choiceId $answerAreaId"}</value>
            }}
          }
        </correctResponse>
        <mapping defaultValue="0.00" lowerBound="0.00" upperBound="2.00">
          {
            correctResponse.map{ case (answerAreaId, choiceId) => {
              <mapEntry mapKey={s"$choiceId $answerAreaId"} mappedValue="0.50"/>
            }}
          }
        </mapping>
      </responseDeclaration>
      <itemBody>
        <gapMatchInteraction responseIdentifier={identifier} shuffle="false">
          <prompt>Complete the sentences about how we taste flavors.<br/></prompt>
          {
            choices.map{ case (id, label) => {
              <gapText identifier={id} matchMax="1">{label}</gapText>
            }}
          }
          <p>Your tongue is covered in <gap identifier="GAP-17995"/>"which tell your <gap identifier="GAP-17996"/>&#xA0;the flavor you are eating. If you eat <gap identifier="GAP-17997"/>, the <gap identifier="GAP-17998"/>" of your tongue tastes the sweet flavor.</p>
        </gapMatchInteraction>
      </itemBody>
    </assessmentItem>

  "interactionJs" should {
    val result = GapMatchInteractionTransformer.interactionJs(qti, QTIManifest.EmptyManifest).get(identifier).getOrElse(throw new IllegalArgumentException("test"))

    "componentType" should {
      "be corespring-drag-and-drop-inline" in {
        (result \ "componentType").as[String] must be equalTo("corespring-drag-and-drop-inline")
      }
    }

    "correctResponse" should {
      val correctResponseResult = (result \ "correctResponse").as[JsObject]

      "map answer area id to an array containing single choice for correct response" in {
        correctResponseResult.keys.map{ key => {
          val value = (correctResponseResult \ key).as[Seq[String]].head
          correctResponse.get(key) must be equalTo(Some(value))
        }}.head
      }
    }

    "model" should {
      val model = (result \ "model").as[JsObject]

      "answerAreas" should {
        val answerAreas = (model \ "answerAreas").as[Seq[JsObject]]

        "contain objects with ids of values" in {
          val ids = answerAreas.map{ obj => (obj \ "id").as[String]}.toSet
          ids diff answerAreaIds.toSet must beEmpty
        }
      }

      "choices" should {
        val responseChoices = (model \ "choices").as[Seq[JsObject]]

        "contain one object for all choice ids" in {
          val responseChoiceIds = responseChoices.map(choice => (choice \ "id").as[String]).toSet
          responseChoiceIds diff choices.keys.toSet must beEmpty
        }

        "contain label corresponding to each choice from qti" in {
          val responseValues = responseChoices.map(choice => (choice \ "id").as[String] -> (choice \ "label").as[String]).toMap
          responseValues.map{ case(id, label) => {
            choices.get(id) must be equalTo(Some(label))
          }}.head
        }

        "contain labelType text" in {
          responseChoices.map{ responseChoice => {
            (responseChoice \ "labelType").as[String] must be equalTo("text")
          }}.head
        }
      }

      "config" should {
        val config = (model \ "config").as[JsObject]

        "shuffle" should {
          "be false" in {
            (config \ "shuffle").as[Boolean] must be equalTo(false)
          }
        }

        "choiceAreaLabel" should {
          "be an empty string" in {
            (config \ "choiceAreaLabel").as[String] must be equalTo("")
          }
        }

        "choiceAreaLayout" should {
          "be equal to horizontal" in {
            (config \ "choiceAreaLayout").as[String] must be equalTo("horizontal")
          }
        }

        "choiceAreaPosition" should {
          "be equal to below" in {
            (config \ "choiceAreaPosition").as[String] must be equalTo("below")
          }
        }
      }
    }

  }

  "transform" should {

    val result = new InteractionRuleTransformer(GapMatchInteractionTransformer).transform(qti)

    "replace <gapMatchInteraction/> with <corespring-drag-and-drop-inline/>" in {
      (result \ "gapMatchInteraction") must beEmpty
      (result \\ "corespring-drag-and-drop-inline").find(inline => (inline \\ "@id").text == identifier) must not beEmpty
    }

  }

}
