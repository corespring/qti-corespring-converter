package org.parcconline.conversion.qti.interactions

import org.corespring.common.file.SourceWrapper
import org.corespring.conversion.qti.interactions.{GraphicGapMatchInteractionTransformer => SuperGraphicGapMatchInteractionTransformer}
import org.parcconline.conversion.qti.util.SVGZConverter
import play.api.libs.json._
import scala.xml._

class GraphicGapMatchInteractionTransformer(sources: Map[String, SourceWrapper]) extends SuperGraphicGapMatchInteractionTransformer {

  private val choiceAreaWidth = 150

  private def choices(json: JsObject) = (json \ "model" \ "choices").as[Seq[JsObject]].map{ choice =>
    choice.deepMerge(Json.obj(
      "label" -> XML.loadString((choice \ "label").as[String]).withoutDimensions.toString
    ))
  }

  private def choiceAreaPosition(json: JsObject) = {
    val maxChoiceWidth = (json \ "model" \ "choices").as[Seq[JsObject]].map{ choice => {
      XML.loadString((choice \ "label").as[String]).attribute("src") match {
        case Some(Text(imageFilename)) => {
          Some((sources.find{ case(filename, _) => filename.endsWith(imageFilename) } match {
            case Some((_, file)) => SVGZConverter.getDimensions(file)
            case _ => throw new Exception(s"Could not find image $imageFilename")
          })._1)
        }
        case _ => None
      }
    }}.flatten.max
    maxChoiceWidth > choiceAreaWidth match {
      case true => "bottom"
      case _ => DefaultChoiceAreaPosition
    }
  }

  private def correctResponses(id: String, qti: Node) =
    (((qti \\ "responseDeclaration").find(n => (n \ "@identifier").text == id) match {
      case Some(node) => node
      case _ => throw new IllegalArgumentException(s"QTI does not contain responseDeclaration for $id")
    }) \\ "value").toSeq.map(n => JsString(n.text.trim)).map { cr =>
      val idHotspotRegex = """([^\s]*) ([^\s]*)""".r
      cr.asOpt[String] match {
        case Some(idHotspotRegex(id, hotspot)) => Json.obj(
          "id" -> id,
          "hotspot" -> hotspot)
        case _ => Json.obj()
      }
    }

  override def interactionJs(qti: Node, manifest: Node) = super.interactionJs(qti, manifest).map{ case(id, json) => {
    val imageFilename = (json \ "model" \ "config" \ "backgroundImage" \ "path").as[String]
    val (width, height) = sources.find{ case(filename, _) => filename.endsWith(imageFilename) } match {
      case Some((_, file)) => SVGZConverter.getDimensions(file)
      case _ => throw new Exception(s"Could not find image $imageFilename")
    }
    id -> (json.deepMerge(Json.obj(
      "feedback" -> Json.obj(
        "correctFeedbackType" -> "none",
        "partialFeedbackType" -> "none",
        "incorrectFeedbackType" -> "none"
      ),
      "model" -> Json.obj(
        "config" -> Json.obj(
          "choiceAreaPosition" -> choiceAreaPosition(json),
          "backgroundImage" -> Json.obj(
            "width" -> width,
            "height" -> height,
            "fixedWidth" -> false
          )
        ),
        "choices" -> choices(json)
      )
    )).as[JsObject] ++ Json.obj("correctResponse" -> correctResponses(id, qti)))
  }}

  implicit class WithoutDimensions(elem: Elem) {

    private def remove(elem: Elem, attribute: String): Elem =
      elem.copy(attributes = elem.attributes.find(_.key == attribute).get.remove(attribute))

    def withoutDimensions = remove(remove(elem, "height"), "width")

  }

}
