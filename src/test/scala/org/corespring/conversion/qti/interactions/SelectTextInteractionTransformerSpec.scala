package org.corespring.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.corespring.conversion.qti.transformers.InteractionRuleTransformer
import org.specs2.mutable.Specification
import play.api.libs.json.{JsArray, JsObject, Json}

import scala.xml.XML

class SelectTextInteractionTransformerSpec extends Specification {

  val identifier = "Q_01"
  val checkIfCorrect = "no"
  val minSelections = 2
  val maxSelections = 10

  def qti(selectionText: Seq[String], selectionType: String) = XML.loadString(s"""
    <assessmentItem>
      <responseDeclaration identifier="${identifier}">
      </responseDeclaration>
      <itemBody>
        <selectTextInteraction responseIdentifier="${identifier}"
            selectionType="${selectionType}" checkIfCorrect="${checkIfCorrect}"
            minSelections="${minSelections.toString}" maxSelections="${maxSelections.toString}"
            >${selectionText.mkString(" ").replaceAll(" , ", ", ")}</selectTextInteraction>
      </itemBody>
    </assessmentItem>
    """)

  val selectionTextWord = Seq("Lorem", "<b><i>ipsum</i></b>", "<correct>dolor</correct>", "sit", "amet,", "consectetur",
    "adipisicing", "<correct>elit</correct>", ",", "sed", "do", "eiusmod", "tempor", "incididunt", "ut", "labore", "et", "dolore", "magna",
    "aliqua.", "Ut", "enim", "ad", "minim", "veniam,", "quis", "nostrud", "exercitation", "ullamco", "laboris", "nisi",
    "ut", "aliquip", "ex", "ea", "commodo", "consequat.", "Duis", "aute", "irure", "dolor", "in", "reprehenderit", "in",
    "voluptate", "velit", "esse", "cillum", "dolore", "eu", "fugiat", "nulla", "pariatur.", "Excepteur", "sint",
    "occaecat", "cupidatat", "non", "proident,", "sunt", "in", "culpa", "qui", "officia", "deserunt", "mollit", "anim",
    "id", "est", "laborum."
  )

  val selectionTextSentence = Seq("""<correct>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua</correct>.""",
    "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
    "<correct>Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur</correct>.",
    "Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  )

//  val input = qti(selectionTextSentence, "sentence"){

  "selectTextInteraction for 'word'" should {

    val wordQti = qti(selectionTextWord, "word")

    val interactionJs =
      SelectTextInteractionTransformer.interactionJs(wordQti, QTIManifest.EmptyManifest).get(identifier)
        .getOrElse(throw new IllegalStateException(s"Missing result for $identifier"))

    val xhtml = new InteractionRuleTransformer(SelectTextInteractionTransformer).transform(wordQti).mkString

    "interactionJs" should {

      "componentType must be equal to 'corespring-select-text'" in {
        (interactionJs \ "componentType").as[String] must be equalTo("corespring-select-text")
      }

      "config" should {

        val config = (interactionJs \ "model" \ "config").as[JsObject]

        "selectionUnit set to 'custom'" in {
          (config \ "selectionUnit").as[String] must be equalTo("custom")
        }

        "maxSelections set to value of maxSelections" in {
          (config \ "maxSelections").as[Int] must be equalTo(maxSelections)
        }

        "label set to empty String" in {
          (config \ "label").as[String] must be equalTo("")
        }

        "availability set to 'all'" in {
          (config \ "availability").as[String] must be equalTo("all")
        }

        "passage wraps all words in <span class='cs-token'/>, removing <correct/>" in {
          (config \ "passage").as[String] must be equalTo(
            selectionTextWord.map(word => s"""<span class="cs-token">$word</span>""")
              .mkString(" ").replaceAll("\\</?correct\\>", ""))
        }

      }

      "allowPartialScoring set to false" in {
        (interactionJs \ "allowPartialScoring").as[Boolean] must be equalTo(false)
      }

      "correctResponse contains indexes of correct selections" in {
        (interactionJs \ "correctResponse" \ "value").as[Seq[Int]] must be equalTo(
          selectionTextWord
            .zipWithIndex
            .filter{ case (word, _) => word.startsWith("<correct")}
            .map{ case (_, index) => index }
        )
      }

      "feedback" should {

        "have correctFeedbackType set to 'default'" in {
          (interactionJs \ "feedback" \ "correctFeedbackType").as[String] must be equalTo("default")
        }

        "have partialFeedbackType set to 'default'" in {
          (interactionJs \ "feedback" \ "partialFeedbackType").as[String] must be equalTo("default")
        }

        "have incorrectFeedbackType set to 'default'" in {
          (interactionJs \ "feedback" \ "incorrectFeedbackType").as[String] must be equalTo("default")
        }

      }

      "partialScoring" should {

        "be set to an array containing an empty object" in {
          (interactionJs \ "partialScoring").as[JsArray] must be equalTo(Json.arr(Json.obj()))
        }

      }

    }

  }

  "selectTextInteraction for 'sentence'" should {

    val sentenceQti = qti(selectionTextSentence, "sentence")

    val interactionJs =
      SelectTextInteractionTransformer.interactionJs(sentenceQti, QTIManifest.EmptyManifest).get(identifier)
        .getOrElse(throw new IllegalStateException(s"Missing result for $identifier"))

    "config" should {

      val config = (interactionJs \ "model" \ "config").as[JsObject]

      "passage wraps all setences in <span class='cs-token'/>, removing <correct/>" in {
        (config \ "passage").as[String] must be equalTo (
          selectionTextSentence.map(word => s"""<span class="cs-token">$word</span>""")
            .mkString(" ").replaceAll("\\</?correct\\>", ""))
      }

    }

    "correctResponse contains indexes of correct selections" in {
      (interactionJs \ "correctResponse" \ "value").as[Seq[Int]] must be equalTo(
        selectionTextSentence
          .zipWithIndex
          .filter{ case (word, _) => word.startsWith("<correct")}
          .map{ case (_, index) => index }
        )
    }


  }


}
