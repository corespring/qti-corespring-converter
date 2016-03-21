package org.corespring.conversion.qti.interactions

import play.api.libs.json.{JsArray, JsString, Json, JsObject}

import scala.xml._
import scala.xml.transform.{RuleTransformer, RewriteRule}

object GapMatchInteractionTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    (qti \\ "gapMatchInteraction").map(implicit node => {

      val correctResponse = (qti \\ "responseDeclaration").find(rd => (rd \ "@identifier").text == (node \ "@responseIdentifier").text)
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

      (node \\ "@responseIdentifier").text -> Json.obj(
        "componentType" -> "corespring-drag-and-drop-inline",
        "correctResponse" -> JsObject(correctResponse.map{ case (key, value) => {
          key -> JsArray(value.map(JsString))
        }}.toSeq),
        "feedback" -> Json.obj(
          "correctFeedbackType" -> "none",
          "partialFeedbackType" -> "none",
          "incorrectFeedbackType" -> "none"
        ),
        "model" -> Json.obj(
          "answerAreaXhtml" -> AnswerAreaTransformer.transform(node).child.mkString.trim,
          "answerAreas" -> (node \\ "gap").map(gap => Json.obj("id" -> (gap \ "@identifier").text)),
          "choices" -> (node \\ "gapText").map(gapText => Json.obj(
            "label" -> gapText.child.mkString,
            "labelType" -> "text",
            "id" -> (gapText \ "@identifier").text,
            "moveOnDrag" -> moveOnDrag(gapText)
          )),
          "config" -> Json.obj(
            "shuffle" -> false,
            "choiceAreaLabel" -> "",
            "choiceAreaLayout" -> "horizontal",
            "choiceAreaPosition" -> "above",
            "removeAllAfterPlacing" -> true
          )
        )
      )}).toMap

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case elem: Elem if elem.label == "gapMatchInteraction" => {
      val identifier = (elem \ "@responseIdentifier").text
      <corespring-drag-and-drop-inline id={ identifier }></corespring-drag-and-drop-inline>
    }
    case _ => node
  }

  private object AnswerAreaTransformer {

    val answerAreaRewriteRule = new RewriteRule {
      override def transform(node: Node) = node.label match {
        case "gapText" => Seq.empty
        case "gap" => <answer-area-inline-csdndi id={(node \ "@identifier").text}></answer-area-inline-csdndi>
          .copy(child = node.child)
        case _ => node
      }
    }

    def transform(node: Node): Node = new RuleTransformer(answerAreaRewriteRule).transform(node).head

  }

}
