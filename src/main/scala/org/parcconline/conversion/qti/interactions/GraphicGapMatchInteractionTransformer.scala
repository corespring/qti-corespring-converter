package org.parcconline.conversion.qti.interactions

import org.corespring.common.file.SourceWrapper
import org.corespring.conversion.qti.interactions.{GraphicGapMatchInteractionTransformer => SuperGraphicGapMatchInteractionTransformer}
import org.parcconline.conversion.qti.util.SVGZConverter
import play.api.libs.json._
import scala.xml._

class GraphicGapMatchInteractionTransformer(sources: Map[String, SourceWrapper]) extends SuperGraphicGapMatchInteractionTransformer {

  private val choiceAreaWidth = 150

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

  override def interactionJs(qti: Node, manifest: Node) = super.interactionJs(qti, manifest).map{ case(id, json) => {
    val imageFilename = (json \ "model" \ "config" \ "backgroundImage" \ "path").as[String]
    val (width, height) = sources.find{ case(filename, _) => filename.endsWith(imageFilename) } match {
      case Some((_, file)) => SVGZConverter.getDimensions(file)
      case _ => throw new Exception(s"Could not find image $imageFilename")
    }
    id -> json.deepMerge(Json.obj(
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
            "height" -> height
          )
        )
      )
    ))
  }}

}
