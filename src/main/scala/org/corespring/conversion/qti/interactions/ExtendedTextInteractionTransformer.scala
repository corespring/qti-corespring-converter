package org.corespring.conversion.qti.interactions

import play.api.libs.json._

import scala.xml._

object ExtendedTextInteractionTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] =
    (qti \\ "extendedTextInteraction").map(implicit node => {
      (node \\ "@responseIdentifier").text -> Json.obj(
        "componentType" -> "corespring-extended-text-entry",
        "model" -> Json.obj(
          "config" -> partialObj(
            "expectedLength" -> optForAttr[JsNumber]("expectedLength"),
            "expectedLines" -> optForAttr[JsNumber]("expectedLines"),
            "maxStrings" -> optForAttr[JsNumber]("maxStrings"),
            "minStrings" -> optForAttr[JsNumber]("minStrings"))))
    }).toMap

  override def transform(node: Node, manifest: Node) = node match {
    case e: Elem if (e.label == "extendedTextInteraction") =>
      <p class="prompt">{ (node \ "prompt").map(_.child).flatten }</p> ++
        <corespring-extended-text-entry id={ (e \ "@responseIdentifier").text }></corespring-extended-text-entry>
    case _ => node
  }

}
