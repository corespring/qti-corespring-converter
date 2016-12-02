package org.corespring.conversion.qti.interactions

import org.corespring.common.util.NumberParsers
import play.api.libs.json._

import scala.xml._

class GraphicGapMatchInteractionTransformer extends InteractionTransformer with NumberParsers {
  val DefaultChoiceAreaPosition = "left"
  val DefaultSnapEnabled = false
  val DefaultSnapSensitivity = 0.2
  val MaximumImageWidth = 430

  private def mapValueToRealImageSize(imageWidth: Int, value: Float): Float = {
    if (imageWidth > MaximumImageWidth) {
      MaximumImageWidth * value / imageWidth
    } else {
      value
    }
  }

  override def transform(node: Node, manifest: Node): Seq[Node] =  {
    val identifier = (node \ "@responseIdentifier").text
    node.label match {
      case "graphicGapMatchInteraction" =>
        node.child.filter(_.label == "prompt").map(n => n.label match {
          case "prompt" => <p class="prompt">{ n.child }</p>
          case _ => n
        }) ++ <corespring-graphic-gap-match id={ identifier }></corespring-graphic-gap-match>
      case _ => node
    }
  }

  override def interactionJs(qti: Node, manifest: Node) = (qti \\ "graphicGapMatchInteraction")
    .map(node => {
      val componentId = (node \ "@responseIdentifier").text.trim

      def cutPathPrefix(path: String) = path.substring(path.lastIndexOf('/') + 1)

      val correctResponses = {
        val values = (responseDeclaration(node, qti) \\ "value").toSeq
        values.nonEmpty match {
          case true => values.map(n => JsString(n.text.trim))
          case _ => (responseDeclaration(node, qti) \\ "mapEntry").toSeq.map(n => JsString((n \ "@mapKey").text.trim))
        }
      }

      def imageWidth = intValueOrZero((node \ "object" \ "@width").mkString)
      def mapValue(value: Float): Float = mapValueToRealImageSize(imageWidth, value)

      def hotspots = {
        def coords(shape: String, s: String) = {
          val coordsArray = s.split(',').map(s => floatValueOrZero(s))
          shape match {
            case "rect" => Json.obj(
              "left" -> mapValue(coordsArray(0)),
              "top" -> coordsArray(1),
              "width" -> mapValue(Math.abs(coordsArray(2) - coordsArray(0))),
              "height" -> Math.abs(coordsArray(3) - coordsArray(1)))
            case "poly" =>
              def xCoords = coordsArray.zipWithIndex.collect { case (x, i) if i % 2 == 0 => x }
              def yCoords = coordsArray.zipWithIndex.collect { case (x, i) if i % 2 == 1 => x }
              def coordPairs = xCoords.zip(yCoords)
              JsArray(coordPairs.map(p => Json.obj("x" -> mapValue(p._1), "y" -> p._2)))
          }
        }
        JsArray(((node \\ "associableHotspot").toSeq).map { n =>
          val shape = (n \ "@shape").text.trim
          Json.obj(
            "id" -> (n \ "@identifier").text.trim,
            "shape" -> shape,
            "coords" -> coords(shape, (n \ "@coords").text.trim))
        })
      }

      def choices = JsArray(((node \\ "gapImg").toSeq).map { n =>
        val imgWidth = mapValue(intValueOrZero((n \ "object" \ "@width").mkString.replaceAll("[^0-9]", "")))
        Json.obj(
          "id" -> (n \ "@identifier").text.trim,
          "label" -> s"<img src='${cutPathPrefix((n \ "object" \ "@data").mkString)}' width='${imgWidth}' height='${(n \ "object" \ "@height").mkString}' />",
          "matchMax" -> intValueOrZero((n \ "@matchMax").text.trim),
          "matchMin" -> intValueOrZero((n \ "@matchMin").text.trim))
      })

      val bgImgWidth = mapValue(intValueOrZero((node \ "object" \ "@width").mkString.replaceAll("[^0-9]", "")))
      val json = Json.obj(
        "componentType" -> "corespring-graphic-gap-match",
        "model" -> Json.obj(
          "config" -> Json.obj(
            "shuffle" -> false,
            "snapEnabled" -> DefaultSnapEnabled,
            "snapSensitivity" -> DefaultSnapSensitivity,
            "choiceAreaPosition" -> ((node \\ "choiceAreaPosition").text match {
              case "" => DefaultChoiceAreaPosition
              case choiceAreaPosition => choiceAreaPosition
            }).toLowerCase,
            "backgroundImage" -> Json.obj(
              "path" -> cutPathPrefix((node \ "object" \ "@data").mkString),
              "width" -> JsNumber(BigDecimal(bgImgWidth)),
              "height" -> JsNumber(intValueOrZero((node \ "object" \ "@height").mkString))),
            "showHotspots" -> JsBoolean(false)),
          "hotspots" -> hotspots,
          "choices" -> choices),
        "feedback" -> Json.obj(
          "correctFeedbackType" -> "default",
          "partialFeedbackType" -> "default",
          "incorrectFeedbackType" -> "default"),
        "correctResponse" -> correctResponses.map { cr =>
          val idHotspotRegex = """([^\s]*) ([^\s]*)""".r
          cr.asOpt[String] match {
            case Some(idHotspotRegex(id, hotspot)) => Json.obj(
              "id" -> id,
              "hotspot" -> hotspot)
            case _ => Json.obj()
          }
        })

      componentId -> json

    }).toMap
}
