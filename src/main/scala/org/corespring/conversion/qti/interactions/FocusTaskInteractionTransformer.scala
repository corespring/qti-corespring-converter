package org.corespring.conversion.qti.interactions

import play.api.libs.json._

import scala.xml._

object FocusTaskInteractionTransformer extends InteractionTransformer {

  override def interactionJs(qti: Node, manifest: Node) = (qti \\ "focusTaskInteraction").map(implicit node => {
    (node \\ "@responseIdentifier").text ->
      Json.obj(
        "componentType" -> "corespring-focus-task",
        "correctResponse" -> Json.obj(
          "value" -> (responseDeclaration(node, qti) \\ "value").map(_.text)),
        "model" -> partialObj(
          "config" -> Some(partialObj(
            "shuffle" -> optForAttr[JsBoolean]("shuffle"),
            "itemShape" -> optForAttr[JsString]("itemShape"),
            "checkIfCorrect" -> optForAttr[JsString]("checkIfCorrect"),
            "minSelections" -> optForAttr[JsNumber]("minSelections"),
            "maxSelections" -> optForAttr[JsNumber]("maxSelections"))),
          "choices" -> Some(JsArray(
            (node \\ "focusChoice").map(choiceNode =>
              Json.obj(
                "label" -> choiceNode.text,
                "value" -> (choiceNode \ "@identifier").text))))))
  }).toMap

  override def transform(node: Node, manifest: Node): Seq[Node] = node match {
    case e: Elem if e.label == "focusTaskInteraction" => {
      val identifier = (e \ "@responseIdentifier").text
      <corespring-focus-task id={ identifier }></corespring-focus-task>.withPrompt(node)
    }
    case _ => node
  }

}