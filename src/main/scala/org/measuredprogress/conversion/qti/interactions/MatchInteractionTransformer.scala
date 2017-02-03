package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.InteractionTransformer
import org.measuredprogress.conversion.qti.util.NamespaceStripper
import play.api.libs.json._

import scala.xml.Node

object MatchInteractionTransformer extends InteractionTransformer with ImageConverter with NamespaceStripper {

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case node: Node if (node.label == "matchInteraction") =>
      <p class="prompt">{ (node \ "prompt").map(_.child).flatten }</p> ++
          <corespring-match id={ (node \\ "@responseIdentifier").text }/>
    case _ => node
  }

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = (qti \\ "matchInteraction").map(implicit node => {
    (node \ "@responseIdentifier").text -> Json.obj(
      "componentType" -> "corespring-match",
      "correctResponse" -> correctResponse(qti),
      "allowPartialScoring" -> false,
      "partialScoring" -> Json.arr(Json.obj()),
      "feedback" -> Json.obj(
        "correctFeedbackType" -> "none",
        "partialFeedbackType" -> "none",
        "incorrectFeedbackType" -> "none"
      ),
      "legacyScoring" -> legacyScoring(qti),
      "model" -> Json.obj(
        "columns" -> {
          val cols: Seq[JsObject] = ((node \\ "simpleMatchSet").tail \\ "simpleAssociableChoice").map(col => Json.obj("labelHtml" -> stripNamespaces(convertObjectsToImages(col.child).toString)))
          Json.obj("labelHtml" -> JsString(stripNamespaces((node \ "prompt").headOption.map(n => convertObjectsToImages(n.child)).getOrElse(Seq.empty).toString))) +: cols
        },
        "rows" -> ((node \\ "simpleMatchSet").head \\ "simpleAssociableChoice").map(row => Json.obj(
          "id" -> (row \ "@identifier").text,
          "labelHtml" -> stripNamespaces(convertObjectsToImages(row.child).toString)
        )),
        "config" -> Json.obj(
          "inputType" -> inputType(qti),
          "shuffle" -> false
        )
      )
    )
  }).toMap

  private def legacyScoring(qti: Node)(implicit node: Node): JsObject = {
    def indexOf(column: String) =
      ((node \\ "simpleMatchSet").tail \\ "simpleAssociableChoice")
        .map{ choice => (choice \ "@identifier").toString }.indexOf(column).toString

    def mappingAttribute(attr: String) = {
      val value = (responseDeclaration(node, qti) \ "mapping" \ s"@$attr").toString
      value.isEmpty match {
        case true => None
        case _ => Some(JsNumber(BigDecimal(value)))
      }
    }

    Json.obj(
      "mapping" -> (responseDeclaration(node, qti) \ "mapping" \\ "mapEntry")
      .filter(mapEntry => (mapEntry \ "@mappedValue").toString.toFloat != 0)
      .foldLeft(Map.empty[String, Map[String, Float]]){ case (acc, mapEntry) => {
        val Array(row, column) = (mapEntry \ "@mapKey").toString.split(" ")
        acc + (row -> (acc.get(row).getOrElse(Map.empty[String, Float]) +
          (indexOf(column) -> (mapEntry \ "@mappedValue").toString.toFloat)))
      }}
    ).deepMerge(partialObj(
      "defaultValue" -> mappingAttribute("defaultValue"),
      "upperBound" -> mappingAttribute("upperBound"),
      "lowerBound" -> mappingAttribute("lowerBound")
    ))
  }

  private def correctResponse(qti: Node)(implicit node: Node) = {
    def matchSet(id: String, answers: Seq[String]): Seq[Boolean] =
      ((node \\ "simpleMatchSet").last \\ "simpleAssociableChoice").map(choice => answers.contains((choice \ "@identifier").text))

    (((node \\ "simpleMatchSet").head \\ "simpleAssociableChoice").zipWithIndex.map{ case (choice, index) => {
      Json.obj(
        "id" -> (choice \ "@identifier").text,
        "matchSet" -> matchSet(
          (choice \ "@identifier").text,
          (responseDeclaration(node, qti) \\ "correctResponse" \\ "value")
            .filter(_.text.trim.startsWith((choice \ "@identifier").text))
            .map(_.text.split(" ").drop(1).toSeq).flatten)
      )
    }})
  }

  private def inputType(qti: Node)(implicit node: Node) = {
    val rows = (responseDeclaration(node, qti) \\ "correctResponse" \\ "value").map(_.text.split(" ").headOption).flatten
    (rows.distinct.size != rows.size) match {
      case true => "checkbox"
      case _ => "radiobutton"
    }
  }


}
