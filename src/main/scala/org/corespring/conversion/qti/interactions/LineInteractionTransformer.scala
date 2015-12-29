package org.corespring.conversion.qti.interactions

import play.api.libs.json._

import scala.xml._

object LineInteractionTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node) = (qti \\ "lineInteraction").map(implicit node => {

    val exhibit = booleanFor("locked", default = false)

    /**
     * Correct Response aren't required if locked="true"
     * or noninteractive="true" is an attribute on the node
     */
    val correctResponse: JsObject = {
      try {
        Json.obj("correctResponse" ->
          (responseDeclaration(node, qti) \ "correctResponse" \ "value").map(_.text).mkString(""))
      } catch {
        case e: Throwable => if (exhibit) Json.obj() else throw e
      }
    }

    val main = Json.obj(
      "componentType" -> "corespring-line",
      "model" -> Json.obj(
        "config" -> partialObj(
          "domain" -> optForAttr[JsNumber]("domain"),
          "range" -> optForAttr[JsNumber]("range"),
          "scale" -> optForAttr[JsNumber]("scale"),
          "domainLabel" -> Some(JsString((node \ "@domain-label").text match {
            case label if (label.length == 1) => ""
            case label => label
          })),
          "rangeLabel" -> Some(JsString((node \ "@range-label").text match {
            case label if (label.length == 1) => ""
            case label => label
          })),
          "tickLabelFrequency" -> optForAttr[JsNumber]("tick-label-frequency"),
          "sigfigs" -> optForAttr[JsNumber]("sigfigs"),
          "initialValues" -> ((node \ "graphline" \\ "point") match {
            case empty: Seq[Node] if empty.isEmpty => None
            case nodes: Seq[Node] => Some(JsArray(nodes.map(n => JsString(n.text))))
          }),
          "initialCurve" -> ((node \ "graphcurve").text match {
            case curve: String if curve.nonEmpty => Some(JsString(curve))
            case _ => None
          }),
          "exhibitOnly" -> Some(JsBoolean(exhibit)),
          "showCoordinates" -> Some(JsBoolean(false)),
          "showInputs" -> Some(JsBoolean(booleanFor("show-inputs") && !exhibit)),
          "showFeedback" -> Some(JsBoolean(false)), // Don't show internal feedback in v1 originated items
          "showLabels" -> Some(JsBoolean(booleanFor("show-labels") && !exhibit)))))

    (node \\ "@responseIdentifier").text -> (main ++ correctResponse)
  }).toMap

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case elem: Elem if elem.label == "lineInteraction" => {
      val identifier = (elem \ "@responseIdentifier").text
      <corespring-line id={ identifier }></corespring-line>
    }
    case _ => node
  }

  private def booleanFor(attribute: String, default: Boolean = true)(implicit node: Node) =
    ((node \\ s"@$attribute").text) match {
      case "true" => true
      case "false" => false
      case _ => default
    }

}
