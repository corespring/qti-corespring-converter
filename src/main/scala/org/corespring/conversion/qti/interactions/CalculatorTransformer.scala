package org.corespring.conversion.qti.interactions

import play.api.libs.json._
import scala.xml._

object CalculatorTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    (qti \\ "csCalculator").map(implicit node => {
      (node \\ "@responseIdentifier").text -> Json.obj(
        "weight" -> 0,
        "title" -> "Calculator",
        "isTool" -> true,
        "componentType" -> "corespring-calculator",
        "model" -> Json.obj(
          "config" -> partialObj(
            "type" -> optForAttr[JsString]("type")
          )))
    }).toMap

  override def transform(node: Node, manifest: Node) = node match {
    case e: Elem if (e.label == "csCalculator") =>
      <p><corespring-calculator id={ (e \ "@responseIdentifier").text }></corespring-calculator></p>
    case _ => node
  }

}
