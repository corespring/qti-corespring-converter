package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json.Reads._

class InlineChoiceInteractionTransformerHMHSpec extends Specification {

  "InlineChoiceInteractionTransformer" should {

    val responseIdentifier = "1"
    val correctResponse = "B"
    val choices = Map("A" -> "choice one", "B" -> "choice two","C" -> "choice three","D" -> "choice four")
    val promptIC =  s"Three of these statements describe possible outcomes of this demonstration.";
    val shuffle = false

    def inlineChoiceQti(responseIdentifier: String = responseIdentifier,
                        correctResponse: String = correctResponse,
                        promptIC: String = promptIC,
                        choices: Map[String, String] = choices,
                        shuffle: Boolean = shuffle
                       ) =
      <assessmentItem identifier="IMNL20E11_hsa_CCSS.1.G.A.3_QUESTION_114680156">
        <responseDeclaration identifier={ responseIdentifier } cardinality="single">
          <correctResponse><value>{ correctResponse }</value></correctResponse>
        </responseDeclaration>
        <itemBody>
            { promptIC }
          <inlineChoiceInteraction responseIdentifier={ responseIdentifier } shuffle={ shuffle.toString }>
            { choices.map { case (id, text) => <inlineChoice identifier={ id }>{ text }</inlineChoice> } }
          </inlineChoiceInteraction>
        </itemBody>
      </assessmentItem>
    val inlineChoiceInteractionResult = InlineChoiceInteractionTransformer.interactionJs(inlineChoiceQti(), QTIManifest.EmptyManifest)

    "transform prompt in Inline Choice Interaction" in {
      val json = inlineChoiceInteractionResult.values.headOption.getOrElse(throw new Exception("There was no result"))
      val prompt = (json \\ "markup")(1).as[String]
      prompt.contains(promptIC)
    }
  }
}