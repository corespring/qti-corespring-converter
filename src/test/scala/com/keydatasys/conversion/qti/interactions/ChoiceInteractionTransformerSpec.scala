package com.keydatasys.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json._

class ChoiceInteractionTransformerSpec extends Specification {

  "ChoiceInteractionTransformer" should {

    val responseIdentifier = "1"
    val correctResponse = "2"
    val choices = Map("1" -> "choice one", correctResponse -> "choice two")
    val rationales = Map("1" -> "This is why choice one", correctResponse -> "This is why choice two")
    val shuffle = false
    val maxChoices = "1"

    def multipleChoiceQti(responseIdentifier: String = responseIdentifier,
                          correctResponse: String = correctResponse,
                          choices: Map[String, String] = choices,
                          shuffle: Boolean = shuffle,
                          rationales: Map[String, String] = rationales,
                          maxChoices: String = maxChoices) =
      <assessmentItem>
        <responseDeclaration identifier={ responseIdentifier } cardinality="single">
          <correctResponse>
            <value>
              { correctResponse }
            </value>
          </correctResponse>
        </responseDeclaration>
        <itemBody>
          <choiceInteraction responseIdentifier={ responseIdentifier } shuffle={ shuffle.toString } maxChoices={ maxChoices }>
            { choices.map { case (id, text) => <simpleChoice identifier={ id }>{ text }</simpleChoice> } }
          </choiceInteraction>
          <choiceRationales responseIdentifier={ responseIdentifier }>
            { rationales.map { case (id, rationale) => <rationale identifier={ id }>{ rationale }</rationale> } }
          </choiceRationales>
        </itemBody>
      </assessmentItem>

    def inlineChoiceQti(responseIdentifier: String = responseIdentifier,
                        correctResponse: String = correctResponse,
                        choices: Map[String, String] = choices,
                        shuffle: Boolean = shuffle,
                        rationales: Map[String, String] = rationales) =
      <assessmentItem>
        <responseDeclaration identifier={ responseIdentifier } cardinality="single">
          <correctResponse><value>{ correctResponse }</value></correctResponse>
        </responseDeclaration>
        <itemBody>
          <inlineChoiceInteraction responseIdentifier={ responseIdentifier } shuffle={ shuffle.toString }>
            { choices.map { case (id, text) => <inlineChoice identifier={ id }>{ text }</inlineChoice> } }
          </inlineChoiceInteraction>
          <inlineChoiceRationales responseIdentifier={ responseIdentifier }>
            { rationales.map { case (id, rationale) => <rationale identifier={ id }>{ rationale }</rationale> } }
          </inlineChoiceRationales>
        </itemBody>
      </assessmentItem>

    val multipleChoiceResult = ChoiceInteractionTransformer.interactionJs(multipleChoiceQti(), QTIManifest.EmptyManifest)
    val inlineChoiceResult = ChoiceInteractionTransformer.interactionJs(inlineChoiceQti(), QTIManifest.EmptyManifest)

    "transform rationales for multiple choice" in {
      val json = multipleChoiceResult.values.headOption.getOrElse(throw new Exception("There was no result"))
      val rationaleResult = (json \ "rationales").as[Seq[JsObject]].map(f => (f \ "choice").as[String] -> (f \ "rationale").as[String]).toMap
      rationaleResult must be equalTo (rationales)
    }

    "transform rationales inline choice" in {
      val json = inlineChoiceResult.values.headOption.getOrElse(throw new Exception("There was no result"))
      val rationaleResult = (json \ "rationales").as[Seq[JsObject]].map(f => (f \ "choice").as[String] -> (f \ "rationale").as[String]).toMap
      rationaleResult must be equalTo (rationales)
    }

  }

}