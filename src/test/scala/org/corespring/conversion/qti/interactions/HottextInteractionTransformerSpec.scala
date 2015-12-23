package org.corespring.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.corespring.conversion.qti.transformers.InteractionRuleTransformer
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.specs2.mutable.Specification
import play.api.libs.json._

import scala.collection.JavaConversions._

class HottextInteractionTransformerSpec extends Specification {

  "HottextInteractionTransformer" should {

    val responseIdentifier = "RESPONSE"
    val choices = Map(
      "A" -> "My alarm clock alights at my window,",
      "B" -> "Just to wish me \"Good day!\"",
      "C" -> "Then my alarm clock flaps its wings",
      "D" -> "And quickly flies away.")
    val correctChoice = "B"
    val maxChoices = 3

    def qti(responseIdentifier: String = responseIdentifier,
            choices: Map[String, String] = choices,
            correctChoice: String = correctChoice) =
      <assessmentItem>
        <responseDeclaration identifier={responseIdentifier} cardinality="multiple" baseType="identifier">
          <correctResponse>
            <value>{correctChoice}</value>
          </correctResponse>
        </responseDeclaration>
        <itemBody>
          <hottextInteraction responseIdentifier={responseIdentifier} maxChoices={maxChoices.toString}>
            {
              choices.map{ case(identifier, text) => {
                <hottext fixed="true" identifier={identifier}>{text}</hottext>
              }}
            }
          </hottextInteraction>
        </itemBody>
      </assessmentItem>

    val interactionJs = HottextInteractionTransformer.interactionJs(qti(), QTIManifest.EmptyManifest).get(responseIdentifier)
      .getOrElse(throw new IllegalStateException(s"Missing result for $responseIdentifier"))

    val xhtml = new InteractionRuleTransformer(HottextInteractionTransformer).transform(qti()).mkString


    "interactionJs" should {

      "config" should {

        val config = (interactionJs \ "model" \ "config").as[JsObject]

        "selectionUnit set to 'custom'" in {
          (config \ "selectionUnit").as[String] must be equalTo("custom")
        }

        "maxSelections set to value of <hottextInteraction maxChoices/>" in {
          (config \ "maxSelections").as[Int] must be equalTo(maxChoices)
        }

        "label set to empty String" in {
          (config \ "label").as[String] must be equalTo("")
        }

        "availability set to 'all'" in {
          (config \ "availability").as[String] must be equalTo("all")
        }

        "passage set to choices wrapped in <span class='cs-token'/>" in {
          val choiceWrapped = choices.values.map{ choice => {
            s"""<span class=" cs-token">$choice</span>"""
          }}.mkString("\n")
          (config \ "passage").as[String] must be equalTo(choiceWrapped)
        }

      }

      "allowPartialScoring set to false" in {
        (interactionJs \ "allowPartialScoring").as[Boolean] must be equalTo(false)
      }

      "correctResponse is an array containing correctChoice's index" in {
        val index = choices.keys.zipWithIndex
          .find{ case (choice, _) => choice == correctChoice }.map(_._2).getOrElse(throw new Exception("Not found!"))
        (interactionJs \ "correctResponse" \ "value").as[Seq[Int]] must be equalTo(Seq(index))
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

    "xhtml" should {

      "contain <corespring-select-text/> node with correct response identifier" in {
        val maybeSelectTextNode = Jsoup.parse(xhtml).getElementsByTag("corespring-select-text").toSet.headOption
        maybeSelectTextNode must not beEmpty
        val selectTextNode: Element = maybeSelectTextNode.get
        selectTextNode.attr("id") must be equalTo(responseIdentifier)
      }

    }

  }

}
