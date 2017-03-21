package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.{ChoiceInteractionTransformer => SuperChoiceInteractionTransformer, InteractionTransformer}
import org.measuredprogress.conversion.qti.util.NamespaceStripper
import play.api.libs.json.{Json, JsArray, JsObject}

import scala.xml.Node


object ChoiceInteractionTransformer extends InteractionTransformer with NamespaceStripper {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = {
    SuperChoiceInteractionTransformer.interactionJs(qti, manifest).map {
      case (id, json) => id -> json.deepMerge(partialObj(
        "feedback" -> ((json \ "componentType").asOpt[String] match {
          case Some("corespring-inline-choice") =>
            Some(JsArray((json \ "model" \ "choices").as[Seq[JsObject]].map { choice =>
              Json.obj("value" -> (choice \ "value").as[String], "feedbackType" -> "default")
            }))
          case _ => None
        }),
        "rationales" -> Some(JsArray((json \ "model" \ "choices").as[Seq[JsObject]].map(c => Json.obj(
          "choice" -> Some(stripNamespaces((c \ "value").as[String]))
        )))),
        "model" -> Some(Json.obj("shuffle" -> false)))
      )
    }
  }


  override def transform(node: Node, manifest: Node): Seq[Node] =
    SuperChoiceInteractionTransformer.transform(node, manifest)
  
}
