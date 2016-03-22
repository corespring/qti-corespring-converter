package org.parcconline.conversion.qti.interactions

import org.corespring.common.file.SourceWrapper
import org.corespring.conversion.qti.interactions.{GraphicGapMatchInteractionTransformer => SuperGraphicGapMatchInteractionTransformer}
import org.parcconline.conversion.qti.util.SVGZConverter
import play.api.libs.json.Json

import scala.xml.Node

class GraphicGapMatchInteractionTransformer(sources: Map[String, SourceWrapper]) extends SuperGraphicGapMatchInteractionTransformer {

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
          "backgroundImage" -> Json.obj(
            "width" -> width,
            "height" -> height
          )
        )
      )
    ))
  }}

}
