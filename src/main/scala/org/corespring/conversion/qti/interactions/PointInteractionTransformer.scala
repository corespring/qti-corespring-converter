package org.corespring.conversion.qti.interactions

import play.api.libs.json._

import scala.xml._

object PointInteractionTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node) = (qti \\ "pointInteraction").map(implicit node => {
    val identifier = (node \\ "@responseIdentifier").text
    identifier -> Json.obj(
      "componentType" -> "corespring-point-intercept",
      "correctResponse" -> (responseDeclaration(node, qti) \ "correctResponse" \ "value").map(_.text),
      "model" -> Json.obj(
        "config" -> partialObj(
          "maxPoints" -> optForAttr[JsNumber]("max-points"),
          "scale" -> optForAttr[JsNumber]("scale"),
          "domain" -> optForAttr[JsNumber]("domain"),
          "range" -> optForAttr[JsNumber]("range"),
          "sigfigs" -> optForAttr[JsNumber]("sigfigs"),
          "domainLabel" -> optForAttr[JsString]("domain-label"),
          "rangeLabel" -> optForAttr[JsString]("range-label"),
          "tickLabelFrequency" -> optForAttr[JsNumber]("tick-label-frequency"),
          "pointLabels" -> optForAttr[JsString]("point-labels"),
          "maxPoints" -> optForAttr[JsString]("max-points"),
          "showInputs" -> optForAttr[JsString]("show-inputs"),
          "locked" -> ((node \\ "@locked").isEmpty match {
            case true => None
            case _ => Some(JsBoolean(true))
          }),
          "showCoordinates" -> Some(JsBoolean(booleanFor("show-coordinates"))),
          "showFeedback" -> Some(JsBoolean(false)) // Don't show internal feedback in v1 originated items
        )))
  }).toMap

  private def booleanFor(attribute: String, default: Boolean = true)(implicit node: Node) =
    ((node \\ s"@$attribute").text) match {
      case "true" => true
      case "false" => false
      case _ => default
    }

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case elem: Elem if elem.label == "pointInteraction" => {
      val identifier = (elem \ "@responseIdentifier").text
      <corespring-point-intercept id={ identifier }></corespring-point-intercept>
    }
    case _ => node
  }

}
