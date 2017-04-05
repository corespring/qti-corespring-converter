package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.{ChoiceInteractionTransformer => SuperChoiceInteractionTransformer, InteractionTransformer}
import org.measuredprogress.conversion.qti.util.NamespaceStripper
import play.api.libs.json.{Json, JsArray, JsObject}

import scala.xml.Node


object ChoiceInteractionTransformer extends InteractionTransformer with NamespaceStripper {

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = {
    SuperChoiceInteractionTransformer.interactionJs(qti, manifest).map {
      case (id, json) =>
        id -> json.deepMerge(Json.obj("model" -> Json.obj("choices" -> JsArray((json \ "model" \ "choices").as[Seq[JsObject]].map { choice =>
          Json.obj("label" -> stripNamespaces((choice \ "label").as[String]))
        }))))
    }
  }

  override def transform(node: Node, manifest: Node): Seq[Node] =
    SuperChoiceInteractionTransformer.transform(node, manifest)
  
}
