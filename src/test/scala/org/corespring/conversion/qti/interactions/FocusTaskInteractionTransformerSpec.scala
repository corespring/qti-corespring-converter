package org.corespring.conversion.qti.interactions

import org.corespring.conversion.qti.transformers.InteractionRuleTransformer
import org.specs2.mutable.Specification
import play.api.libs.json.JsObject

import scala.util.Random


class FocusTaskInteractionTransformerSpec extends Specification {

  val identifier = "Q_01"
  val prompt = "This is the best item ever!"
  val choices = Map("A" -> "Option A", "B" -> "Option B", "C" -> "Option C")
  val shuffle = false
  val itemShape = "circle"
  val minSelections = 2
  val maxSelections = 3
  val checkIfCorrect = "yes"

  val correctResponses = Random.shuffle(choices.keys).toSeq

  def qti(correctResponses: Seq[String]) =
    <assessmentItem>
      <responseDeclaration identifier={ identifier }>
        <correctResponse>
          { correctResponses.map(response => <value>{ response }</value>) }
        </correctResponse>
      </responseDeclaration>
      <itemBody>
        <focusTaskInteraction responseIdentifier={ identifier } checkIfCorrect={ checkIfCorrect } minSelections={ minSelections.toString } maxSelections={ maxSelections.toString } shuffle={ shuffle.toString } itemShape={ itemShape }>
          <prompt>{ prompt }</prompt>
          { choices.map { case (identifier, label) => { <focusChoice identifier={ identifier }>{ label }</focusChoice> } } }
        </focusTaskInteraction>
      </itemBody>
    </assessmentItem>

  "FocusTaskInteractionTransformer" should {

    val input = qti(correctResponses)
    val componentsJson = FocusTaskInteractionTransformer.interactionJs(input, ItemTransformer.EmptyManifest)
    val output = new InteractionRuleTransformer(FocusTaskInteractionTransformer).transform(input)

    val interactionResult =
      componentsJson.get(identifier).getOrElse(throw new RuntimeException(s"No component called $identifier"))

    val model = interactionResult \ "model"
    val config = model \ "config"

    "return the correct component type" in {
      (interactionResult \ "componentType").as[String] must be equalTo "corespring-focus-task"
    }

    "returns correct correct response" in {
      (interactionResult \ "correctResponse" \ "value").as[Seq[String]] must be equalTo correctResponses
    }

    "returns correct choices" in {
      val resultChoices = (model \ "choices").as[Seq[JsObject]]
        .map(choice => (choice \ "value").as[String] -> (choice \ "label").as[String])
      resultChoices diff choices.toSeq must beEmpty
    }

    "returns correct shuffle" in {
      (config \ "shuffle").as[Boolean] must be equalTo shuffle
    }

    "returns correct itemShape" in {
      (config \ "itemShape").as[String] must be equalTo itemShape
    }

    "returns correct minSelections" in {
      (config \ "minSelections").as[Int] must be equalTo minSelections
    }

    "returns correct maxSelections" in {
      (config \ "maxSelections").as[Int] must be equalTo maxSelections
    }

    "returns correct checkIfCorrect" in {
      (config \ "checkIfCorrect").as[String] must be equalTo checkIfCorrect
    }

    "returns correct prompt" in {
      (output \\ "p").find(n => n.text == prompt) must not beEmpty
    }

  }

}
