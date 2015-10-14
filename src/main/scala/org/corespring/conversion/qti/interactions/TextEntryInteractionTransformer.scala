package org.corespring.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.FeedbackBlockTransformer.belongsToTextEntry
import org.corespring.conversion.qti.interactions.equation.DomainParser
import org.corespring.conversion.qti.transformers.InteractionRuleTransformer
import play.api.libs.json._

import scala.xml._

object TextEntryInteractionTransformer extends Transformer {

  def transform(qti: Node, manifest: Node): Node =
    new InteractionRuleTransformer(new TextEntryInteractionTransformer(qti)).transform(qti, manifest).head

}

case class TextEntryInteractionTransformer(qti: Node) extends InteractionTransformer with DomainParser {

  val equationRegex = "eqn[:]?(.*)?".r

  val DefaultAnswerBlankSize: Int = 5

  def feedbackBlocks(node: Node, qti: Node): Seq[Node] = {
    (node \ "@responseIdentifier").text match {
      case "" => throw new IllegalArgumentException("Node does not have a responseIdentifier")
      case identifier: String => {
        (qti \\ "feedbackBlock").filter(n => (n \ "@outcomeIdentifier").text == s"responses.${identifier}.value")
      }
    }
  }

  override def interactionJs(qti: Node, manifest: Node) = (qti \\ "textEntryInteraction").map(implicit node => {
    val responseDeclarationNode = responseDeclaration(node, qti)
    val fbBlocks = feedbackBlocks(node, qti)
    val popupFeedback = (node \ "@popupFeedback").text == "true"
    val correctFeedbacks = popupFeedback match {
      case true => fbBlocks.filter(_.attribute("incorrectResponse").isEmpty).map(fb => Json.obj(
        "answer" -> (fb \ "@identifier").text,
        "feedback" -> fb.child.mkString.trim))
      case false => Seq[JsObject]()
    }

    val incorrectFeedback = popupFeedback match {
      case true => fbBlocks.find(!_.attribute("incorrectResponse").isEmpty).map(fb => fb.child.mkString.trim)
      case false => Some("")
    }

    val correctResponses = (responseDeclarationNode \ "correctResponse" \\ "value").map(_.text).toSet
    val answerBlankSize: Int = (node \ "@expectedLength").text.toIntOption.getOrElse(DefaultAnswerBlankSize)

    (node \ "@responseIdentifier").text -> partialObj(
      "weight" -> Some(JsNumber(1)),
      "componentType" -> Some(isEquation(node, qti) match {
        case true => JsString("corespring-function-entry")
        case _ => JsString("corespring-text-entry")
      }),
      "model" -> Some(Json.obj(
        "answerBlankSize" -> answerBlankSize,
        "answerAlignment" -> "left")),
      "feedback" -> Some(Json.obj(
        "correctFeedbackType" -> JsString(if (popupFeedback) "default" else "none"),
        "incorrectFeedbackType" -> JsString(if (popupFeedback) "default" else "none"))),
      isEquation(node, qti) match {
        case true => "correctResponse" -> Some(Json.obj(
          "equation" -> JsString(correctResponses.head)) ++ equationConfig(responseDeclarationNode).getOrElse(Json.obj()))
        case _ => "correctResponses" -> Some(Json.obj(
          "award" -> 100,
          "values" -> JsArray(correctResponses.map(JsString(_)).toSeq),
          "ignoreWhitespace" -> true,
          "ignoreCase" -> true,
          "feedback" -> Json.obj(
            "type" -> (if (popupFeedback) "default" else "none"),
            "specific" -> correctFeedbacks)))
      },
      "incorrectResponses" -> Some(Json.obj(
        "award" -> 0,
        "feedback" -> Json.obj(
          "type" -> (if (popupFeedback) "default" else "none"),
          "value" -> incorrectFeedback))))
  }).toMap

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case e: Elem if e.label == "textEntryInteraction" => isEquation(node, qti) match {
      case true => <corespring-function-entry id={ (node \ "@responseIdentifier").text } class={ if ((node \ "@popupFeedback").text == "true") "popupFeedback" else "" }></corespring-function-entry>
      case false => <corespring-text-entry id={ (node \ "@responseIdentifier").text } class={ if ((node \ "@popupFeedback").text == "true") "popupFeedback" else "" }></corespring-text-entry>
    }

    case e: Elem if e.label == "feedbackBlock" => belongsToTextEntry(node, qti) match {
      case true => NodeSeq.Empty
      case false => e
    }

    case _ => node
  }

  private def isEquation(node: Node, qti: Node) = {
    val baseType = node.label match {
      case "responseDeclaration" => (node \ "@baseType").text
      case "textEntryInteraction" => (responseDeclaration(node, qti) \ "@baseType").text
      case _ => false
    }

    baseType match {
      case equationRegex(_*) => true
      case "line" => true
      case _ => false
    }
  }

  private def equationConfig(responseDeclaration: Node): Option[JsObject] = {
    (responseDeclaration \ "@baseType").text match {
      case equationRegex(params) if Option(params).isDefined => {
        val values = params.split(" ").map(param => {
          param.split(":") match {
            case Array(key: String, value: String, _*) => Some(key -> (key match {
              case "domain" => parseDomain(value)
              case "sigfigs" => JsNumber(value.toInt)
              case _ => JsString(value)
            }))
            case _ => None
          }
        }).flatten.toSeq
        Some(JsObject(values))
      }
      case _ => None
    }
  }

  implicit class StringWithIntOption(string: String) {
    // Tries to convert a String to an Integer. Returns Some[Int] if successful, None otherwise.
    def toIntOption: Option[Int] = {
      try {
        Some(string.toInt)
      } catch {
        case e: Exception => None
      }
    }
  }

}
