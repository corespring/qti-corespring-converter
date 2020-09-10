package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json._
import play.api.libs.json.Reads._

class ChoiceInteractionTransformerHMHSpec extends Specification {

  "ChoiceInteractionTransformer" should {

    val responseIdentifier = "1"
    val correctResponse = "B"
    val choices = Map("A" -> "choice one", "B" -> "choice two","C" -> "choice three","D" -> "choice four")
    val rationales = Map("A" -> "This is why choice one", "B" -> "This is why choice three","C" -> "This is why choice two","D" -> "This is why choice four")
    val promptMC =  "Three of these statements describe possible outcomes of this demonstration. Which of the following is\n<strong>not</strong>\na possible result?";
    val promptIC = "<p>Identify the missing word in this famous quote from Shakespeare's Richard III.</p>"
    val shuffle = false
    val maxChoices = "1"

    def multipleChoiceQti(responseIdentifier: String = responseIdentifier,
                          correctResponse: String = correctResponse,
                          promptMC: String = promptMC,
                          choices: Map[String, String] = choices,
                          shuffle: Boolean = shuffle,
                          rationales: Map[String, String] = rationales,
                          maxChoices: String = maxChoices) =
      <assessmentItem identifier="IMNL20E11_hsa_CCSS.1.G.A.3_QUESTION_114680157">
        <responseDeclaration identifier={ responseIdentifier } cardinality="single">
          <correctResponse>
              { correctResponse.split(',').map { case (text) => <value>{ text }</value> } }
          </correctResponse>
        </responseDeclaration>
        <itemBody>
          <choiceInteraction responseIdentifier={ responseIdentifier } shuffle={ shuffle.toString } maxChoices={ maxChoices }>
            <prompt>
              { promptMC }
            </prompt>
            { choices.map { case (id, text) => <simpleChoice identifier={ id }>{ text } <feedbackInline identifier={ id } showHide="show" outcomeIdentifier="FEEDBACK">{ text }</feedbackInline></simpleChoice> } }
          </choiceInteraction>
        </itemBody>
      </assessmentItem>

    def inlineChoiceQti(responseIdentifier: String = responseIdentifier,
                        correctResponse: String = correctResponse,
                        promptIC: String = promptIC,
                        choices: Map[String, String] = choices,
                        shuffle: Boolean = shuffle,
                        rationales: Map[String, String] = rationales) =
      <assessmentItem identifier="IMNL20E11_hsa_CCSS.1.G.A.3_QUESTION_114680156">
        <responseDeclaration identifier={ responseIdentifier } cardinality="single">
          <correctResponse><value>{ correctResponse }</value></correctResponse>
        </responseDeclaration>
        <itemBody>
          <inlineChoiceInteraction responseIdentifier={ responseIdentifier } shuffle={ shuffle.toString }>
            <prompt>
              { promptIC }
            </prompt>
            { choices.map { case (id, text) => <inlineChoice identifier={ id }>{ text }</inlineChoice> } }
          </inlineChoiceInteraction>
        </itemBody>
      </assessmentItem>

    val multipleChoiceResult = ChoiceInteractionTransformer.interactionJs(multipleChoiceQti(), QTIManifest.EmptyManifest)
    val inlineChoiceResult =    ChoiceInteractionTransformer.interactionJs(inlineChoiceQti(), QTIManifest.EmptyManifest)

    "transform choices for multiple choice" in {
      val json = multipleChoiceResult.values.headOption.getOrElse(throw new Exception("There was no result"))
      val choiceResult = (json \ "config" \ "models" \\ "choices")(0).as[Seq[JsObject]].map(f => (f \ "value").as[String] -> (f \ "label").as[String]).toMap
      choiceResult must be equalTo (choices)
    }

    "transform choices for inline choice" in {
      val json = inlineChoiceResult.values.headOption.getOrElse(throw new Exception("There was no result"))
      val choiceResult = (json \ "config" \ "models" \\ "choices")(0).as[Seq[JsObject]].map(f => (f \ "value").as[String] -> (f \ "label").as[String]).toMap
      choiceResult must be equalTo (choices)
    }

    "transform prompt in multiple choice" in {
      val json = multipleChoiceResult.values.headOption.getOrElse(throw new Exception("There was no result"))
      val prompt = (json \\ "prompt")(0).as[String]
      prompt must be equalTo (promptMC)
    }

    "transform prompt in inline choice" in {
      val json = inlineChoiceResult.values.headOption.getOrElse(throw new Exception("There was no result"))
      val prompt = (json \\ "prompt")(0).as[String]
      prompt must be equalTo (promptIC)
    }
  }

}