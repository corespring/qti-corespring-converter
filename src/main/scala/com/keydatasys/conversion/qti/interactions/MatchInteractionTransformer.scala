package com.keydatasys.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json._

import scala.collection.immutable.TreeMap
import scala.xml._

object MatchInteractionTransformer extends InteractionTransformer {

  val DefaultCornerText = ""

  override def transform(node: Node, manifest: Node) = node match {
    case node: Node if (node.label == "matchInteraction") =>
      <p class="prompt">{ (node \ "prompt").map(_.child).flatten }</p> ++
          <corespring-match id={ (node \\ "@responseIdentifier").text }/>
    case _ => node
  }

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = (qti \\ "matchInteraction").map(implicit node => {
    val missing = missingAnswers(node)(qti)
    missing.nonEmpty match {
      case true => println(s"Interaction ${(qti \ "@identifier").text}, <matchInteraction/> ${(node \\ "@responseIdentifier").text} missing correctResponses for ${missing.mkString(", ")}")
      case _ => {}
    }
    (node \ "@responseIdentifier").text -> Json.obj(
      "componentType" -> "corespring-match",
      "correctResponse" -> answers(qti)(node),
      "model" -> Json.obj(
        "columns" -> (Json.obj("labelHtml" -> ((node \ "cornerText").text.toString match {
          case empty if (empty.isEmpty) => DefaultCornerText
          case nonEmpty: String => nonEmpty
        })) +: columns.values.map(text => Json.obj("labelHtml" -> text)).toSeq),
        "rows" -> rows.map { case (id, text) => Json.obj("id" -> id, "labelHtml" -> text) }.toSeq,
        "answerType" -> (if (columns.values.find(_.toLowerCase.contains("true")).nonEmpty) "TRUE_FALSE" else "YES_NO"),
        "config" -> Json.obj(
          "inputType" -> (qti \\ "responseDeclaration").find(rd => (rd \ "@identifier").text == (node \ "@responseIdentifier").text).map(rd => ((rd \ "@cardinality").text match {
            case "multiple" => "checkbox"
            case _ => "radiobutton"
          })),
          "layout" -> layout,
          "shuffle" -> false
        )))
  }).toMap

  private def columns(implicit node: Node) = filter("Col.*", (choices, acc) => choices.size > acc.size)
  private def rows(implicit node: Node) = filter("Row.*", (choices, acc) => choices.size <= acc.size)
  private def layout(implicit node: Node) =
    s"${numberStrings.get(columns.size + 1).getOrElse(throw new IllegalArgumentException("Invalid number of columns"))}-columns"
  private def answers(qti: Node)(implicit node: Node) = {
    (qti \\ "responseDeclaration").find(rd => (rd \ "@identifier").text == (node \ "@responseIdentifier").text)
      .map(rd => (rd \ "correctResponse" \ "value").toSeq.map(_.text)).getOrElse(Seq.empty)
      .foldLeft(Map.empty[String, Seq[String]]) {
      case (acc, text) => text.split(" ") match {
        case Array(one, two) => acc.get(one) match {
          case Some(list) => acc + (one -> (list :+ two))
          case _ => acc + (one -> Seq(two))
        }
        case _ => throw new IllegalArgumentException("Whoa")
      }
    }.map {
      case (row, cols) => {
        Json.obj("id" -> row, "matchSet" -> columns.keySet.toSeq.map(cols.contains(_)))
      }
    }.toSeq
  }

  private def filter(regex: String, comparator: (Seq[Node], Seq[Node]) => Boolean)(implicit node: Node): Map[String, String] =
    TreeMap((node \ "simpleMatchSet").find(matchSet =>
      (matchSet \ "simpleAssociableChoice").find(choice =>
        regexMatch(regex, (choice \ "@identifier").text)).nonEmpty).map(_ \ "simpleAssociableChoice")
      .getOrElse((node \ "simpleMatchSet").foldLeft(Seq.empty[Node])((acc, n) =>
      (n \ "simpleAssociableChoice") match {
        case choices if comparator(choices, acc) => choices
        case _ => acc
      })).map(choice => (choice \ "@identifier").text -> choice.child.mkString).toMap.toArray: _*)

  private def regexMatch(regex: String, string: String) = {
    val regexX = regex.r
    string match {
      case regexX(_*) => true
      case _ => false
    }
  }

  private def missingAnswers(node: Node)(implicit qti: Node): Seq[String] = {
    val id = (node \ "@responseIdentifier").text
    val rowIds = ((node \\ "simpleMatchSet").head \\ "simpleAssociableChoice").map(n => (n \ "@identifier").toString).toSeq
    val correctIds = (qti \ "responseDeclaration").find(n => (n \ "@identifier").text == id).map(n => {
      (n \\ "value").map(_.text.split(" ").head)
    }).getOrElse(Seq.empty)
    rowIds.filterNot(id => correctIds.contains(id))
  }

  private val numberStrings = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")

}
