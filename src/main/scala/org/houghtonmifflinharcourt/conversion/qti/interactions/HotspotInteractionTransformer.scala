package org.houghtonmifflinharcourt.conversion.qti.interactions

import java.util.UUID.randomUUID

import org.corespring.conversion.qti.interactions.pie.InteractionTransformer
import play.api.libs.json._

import scala.xml._

object HotspotInteractionTransformer extends InteractionTransformer {
  override def interactionJs(qti: Node, manifest: Node) = (qti)
    .map(implicit qtinode => {

      val componentId = (qti \ "@identifier").text.trim
      var polyArray :JsArray = JsArray.empty;
      var rectArray :JsArray = JsArray.empty;
      var markupString : String = ""

      def correctResponses(node : Node): Seq[JsString] = {
        val values: Seq[Node] = (responseDeclaration(node, qti) \\ "value").toSeq
        values.map(n => JsString(n.text.trim))
      }
      def prompt(node: Node): String = {
        (node \ "prompt").length match {
          case length: Int if length == 1 => (removeNamespaces(node) \\ "prompt").map{ n => n.child.mkString.trim }(0)
          case _ => ""
        }
      }
      def maxSelections(node: Node): Int = {
        ((node \\ "@maxChoices").text) match {
          case text: String => (node \\ "@maxChoices").text.toInt
          case _ => 1 //if ((node \\ "@maxChoices").text == "1") "radio" else "checkbox"
        }
      }
      def isCorrect(node : Node, parentNode: Node ): Boolean = {
        val choiceText = (node \ "@identifier").text;
        correctResponses(parentNode).indexOf(JsString(choiceText)) match {
          case -1 =>  false
          case _ =>  true
        }
      }
      def points(coord: String): JsArray = {
        var arr :JsArray = JsArray.empty;
        if(coord.length >0) {
          {
            val result = coord.split(",")
            val sid = 0
            for (sid <- 0 until result.length by 2){
                  val x= result(sid);
              val y= result(sid+1);
              arr = arr :+ Json.obj(
                "x" -> x,
                "y" -> y
              )
            }
          }
        }
        arr
      }
      def getShapes(node:Node) = {
        var shapes : Map[String,JsArray] = Map.empty
        shapes = shapes + ("polygons" -> getPolyObject(node) , "rectangles" -> getRectObject(node)
        )
        shapes
      }
      def getPolyObject(n:Node): JsArray = {
        (qti \\ "hotspotChoice").map { node =>
          (node \ "@shape").text match {
            case "poly" =>  polyArray = polyArray :+ Json.obj(
              "id" -> (node \ "@identifier").text,
              "points" -> points(((node \ "@coords").text)),
              "correct" -> isCorrect(node,n)
            )
            case _ => polyArray
          }

        }
        polyArray
      }
      def getRectObject(n:Node): JsArray = {
        (n \\ "hotspotChoice").map { node =>
          (node \ "@shape").text match {
            case "rect" =>  rectArray = rectArray :+ Json.obj(
              "id" -> (node \ "@identifier").text,
              "points" -> points(((node \ "@coords").text)),
              "correct" -> isCorrect(node,n)
            )
            case _ => rectArray
          }

        }
        rectArray
      }
      def markup(): String = {
        ((qti \\ "hotspotInteraction")).map{ n =>
          (n.label match {
            case "hotspotInteraction" =>  markupString += "<br/><hotspot-element id=\"" + (n \ "@responseIdentifier").text + "\"></hotspot-element>"
          })
        }
        markupString
      }
      def isMultipleCorrect(): Boolean = {
          (((qti \\ "responseDeclaration")(0) \\ "correctResponse")(0) \\ "value").length match {
          case length: Int if length > 1 => true
          case _ => false
        }
      }
      val json = Json.obj(
        "id" -> randomUUID,
        "name" -> (qti \ "@identifier").text,
        "collectionIds" -> Json.toJson(List("test").distinct),
        "config" -> Json.obj(
          "markup" ->  markup(),//JsString("<hotspot-element id=\"" + (qti \ "@responseIdentifier").text + "\"></hotspot-element>"),
          "models" ->  JsArray((qti \\ "hotspotInteraction").map { n =>
            Json.obj(
              "id" -> (n \ "@responseIdentifier").text,
              "maxSelections" -> maxSelections(n),
              "partialScoring" -> false,
              "rationale" -> "",
              "prompt" -> prompt(n),
              "element" -> JsString("hotspot-element"),
              "promptEnabled" -> true,
              "mode" -> "gather",
              "dimensions" -> "",
              "imageUrl" -> ((n \\ "object")(0) \ "@data").text,
              "hotspotColor" -> "rgba(137, 183, 244, 0.65)",
              "hotspotList" -> "rgba(137, 183, 244, 0.65)",
              "outlineColor" -> "blue",
              "outlineList" -> "blue",
              "multipleCorrect" -> isMultipleCorrect(),
              "shapes" -> getShapes(n)
                /*JsArray((qti \\ "hotspotChoice").map { m =>
               Json.obj(
                  "id" -> (m \ "@identifier").text,
                  "points" -> points(((m \ "@coords").text)) ,
                  "shape" -> (m \ "@shape").text
                )}),*/
             )
          }),
          "elements" -> Json.obj(
            "hotspot-element" -> "@pie-element/hotspot@latest"
          )),
        "searchMetaData" -> Json.obj(
          "internal_author" -> "",
          "internal_status" -> "",
          "hmh_standard" -> JsArray(),
          "hmh_locale" -> "en_US",
          "hmh_cognitiveDemand" -> "",
          "hmh_depthOfKnowledge" -> ""
        )
      )
      componentId -> json
    }).toMap
}
