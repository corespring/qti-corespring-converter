package org.parcconline.conversion.qti.interactions

import com.keydatasys.conversion.qti.util.EntityEscaper
import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json._

import scala.xml.Node

object MatchInteractionTransformer extends InteractionTransformer with EntityEscaper {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    (qti \\ "matchInteraction").map(implicit node => {
      val correctResponse: Map[String, Seq[String]] = (qti \\ "responseDeclaration").find(rd => (rd \ "@identifier").text == (node \ "@responseIdentifier").text)
        .map(rd => (rd \ "correctResponse" \ "value").foldLeft(Map.empty[String, Seq[String]]){ case(acc, value) => {
        value.text.split(" ") match {
          case Array(value, key) => acc.get(key) match {
            case Some(list) => acc + (key -> (list :+ value))
            case _ => acc + (key -> Seq(value))
          }
          case _ => throw new Exception(s"Invalid response declaration format for ${(node \ "@responseIdentifier")}")
        }
      }}).getOrElse(Map.empty[String, Seq[String]])

      def moveOnDrag(choice: Node) = {
        def choiceIsSolutionForMultipleCategories(choice: Node) = {
          val id = (choice \ "@identifier").text
          correctResponse.count{ case(_, values) => values.contains(id) } > 1
        }
        def anyChoiceIsSolutionForMultipleCategories = {
          val answers = correctResponse.values.flatten.toSeq
          answers.length != answers.distinct.length
        }
        !anyChoiceIsSolutionForMultipleCategories
      }

      (node \ "@responseIdentifier").text -> Json.obj(
        "componentType" -> "corespring-dnd-categorize",
        "correctResponse" -> JsObject({
          correctResponse.map{ case (key, value) => {
            key -> JsArray(value.map(JsString))
          }}.toSeq
        }),
        "feedback" -> Json.obj(
          "correctFeedbackType" -> "none",
          "partialFeedbackType" -> "none",
          "incorrectFeedbackType" -> "none"
        ),
        "allowPartialScoring" -> false,
        "partialScoring" -> Json.obj(
          "sections" -> ((node \ "simpleMatchSet").last \ "simpleAssociableChoice").map{ choice =>
            Json.obj(
              "catId" -> (choice \ "@identifier").text,
              "partialScoring" -> Json.obj("numberOfCorrect" -> 1, "scorePercentage" -> 0))
          }
        ),
        "allowWeighting" -> true,
        "weighting" -> JsObject(((node \ "simpleMatchSet").last \ "simpleAssociableChoice").map{ choice =>
          (choice \ "@identifier").text -> JsNumber(1) }),
        "model" -> Json.obj(
          "categories" -> ((node \ "simpleMatchSet").last \ "simpleAssociableChoice").map{ choice => Json.obj(
            "id" -> (choice \ "@identifier").text,
            "label" -> encodeSafeEntities(choice.child.mkString)
          )},
          "choices" -> ((node \ "simpleMatchSet").head \ "simpleAssociableChoice").map{ choice => Json.obj(
            "id" -> (choice \ "@identifier").text,
            "label" -> encodeSafeEntities(choice.child.mkString),
            "moveOnDrag" -> moveOnDrag(choice)
          )},
          "config" -> Json.obj(
            "shuffle" -> false,
            "answerAreaPosition" -> "above",
            "categoriesPerRow" -> 1,
            "choicesPerRow" -> 2,
            "choicesLabel" -> "",
            "removeAllAfterPlacing" -> true
          )
        )
      )
    }).toMap

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case node: Node if (node.label == "matchInteraction") =>
      <corespring-dnd-categorize id={ (node \\ "@responseIdentifier").text }></corespring-dnd-categorize>
    case _ => node
  }

}
