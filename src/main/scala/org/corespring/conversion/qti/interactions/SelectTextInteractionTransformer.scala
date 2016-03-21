package org.corespring.conversion.qti.interactions

import play.api.libs.json._

import scala.util.matching.Regex
import scala.xml._

class SelectTextInteractionTransformer extends InteractionTransformer {

  object Defaults {
    val shuffle = false
  }

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    (qti \\ "selectTextInteraction").map{ implicit node => (node \ "@responseIdentifier").text -> Json.obj(
      "componentType" -> "corespring-select-text",
      "model" -> Json.obj(
        "config" -> Json.obj(
          "selectionUnit" -> "custom",
          "maxSelections" -> (node \ "@maxSelections").text.toInt,
          "label" -> "",
          "availability" -> "all",
          "passage" -> choices.map(_._1).map(choice => s"""<span class="cs-token">$choice</span>""").mkString(" ")
        )
      ),
      "allowPartialScoring" -> false,
      "correctResponse" -> Json.obj(
        "value" -> choices.zipWithIndex.filter{ case ((choice, correct), index) => correct }
          .map { case ((choice, correct), index) => index }
      ),
      "feedback" -> Json.obj(
        "correctFeedbackType" -> "default",
        "partialFeedbackType" -> "default",
        "incorrectFeedbackType" -> "default"
      ),
      "partialScoring" -> Json.arr(Json.obj())
    )}.toMap

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case elem: Elem if elem.label == "selectTextInteraction" => {
      val identifier = (elem \ "@responseIdentifier").text
      <corespring-select-text id={ identifier }></corespring-select-text>
    }
    case _ => node
  }


  private def choices(implicit node: Node): Seq[(String, Boolean)] = {
    def isCorrect(string: String) = string.indexOf("<correct>") >= 0
    def stripCorrectness(string: String) =
      string.replaceAll("<[/]*correct>", "")

    def removeBlockLevelElements(fromText: String) = {
      val validTags = List("br", "p", "correct", "b", "i", "u", "strong", "span", "small", "img", "a", "sub", "sup")
      def isValid(tag: String) = {
        val strippedTag = new Regex("<[\\s/]*(\\S+)[^>]*?>", "tag").findFirstMatchIn(tag) match {
          case Some(mm) => mm.group("tag")
          case _ => ""
        }
        validTags.contains(strippedTag)
      }
      s"<.*?>".r.replaceAllIn(fromText, { m =>
        if (isValid(m.toString)) m.toString else ""
      })
    }

    def fixLineBreaks(fromText: String) = {
      val preProcess = s"<.*?>".r.replaceAllIn(fromText, { m =>
        m.toString match {
          case "<br>" => ""
          case "</br>" => "<br/>"
          case "<p>" => ""
          case "</p>" => "<p/>"
          case _ => m.toString
        }
      })
      s"<.*?>".r.replaceAllIn(preProcess, { m =>
        m.toString match {
          case "<p/>" => "<p> </p>"
          case "<br/>" => "<br> </br>"
          case _ => m.toString
        }
      })
    }

    val lineBreaksFixedText = fixLineBreaks(clearNamespace(node.child).mkString)
    val text = removeBlockLevelElements(lineBreaksFixedText)
    val choices = optForAttr[JsString]("selectionType") match {
      case Some(selection) if selection.equals(JsString("word")) => TextSplitter.words(text)
      case _ => TextSplitter.sentences(text)
    }

    choices.map(choice => (stripCorrectness(choice), isCorrect(choice)))
  }

}

object TextSplitter {

  def sentences(s: String): Seq[String] = {
    val regExp = new Regex("(?s)(.*?[.!?]([^ \\t])*)", "match")
    // Filter out names like Vikram S. Pandit as they break the sentence parsing
    val namesParsed = "([A-Z][a-z]+ [A-Z])\\.( [A-Z][a-z]+)".r.replaceAllIn(s, "$1&#46;$2")
    regExp.findAllMatchIn(namesParsed).map({ m => m.group("match").trim }).toList
  }

  def words(s: String): Seq[String] = {
    def getWordsWithXmlTags(node: Node, path: Seq[Node]): Seq[String] = {
      if (node.isInstanceOf[Text]) {
        val l = path.map(_.label).dropRight(1).reverse
        def textWithNeighbouringTags(left: Seq[String], res: String): String = {
          if (left.isEmpty) {
            res
          } else {
            val elem = left.head
            s"<${elem}>${textWithNeighbouringTags(left.tail, res)}</${elem}>"
          }
        }

        val textWithTags = textWithNeighbouringTags(l, node.text.trim)
        s"\\S+".r.findAllIn(textWithTags).toList
      } else {
        node.child.map { n => getWordsWithXmlTags(n, Seq(node) ++ path) }.flatten
      }
    }
    val inputXml = XML.loadString(s"<span>${s}</span>")
    getWordsWithXmlTags(inputXml, Seq())
  }

}