package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.{InteractionTransformer, ChoiceInteractionTransformer => SuperChoiceInteractionTransformer}
import org.measuredprogress.conversion.qti.util.NamespaceStripper
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsArray, JsObject, Json}
import play.api.libs.json.Json._

import scala.xml.Node
import org.corespring.macros.DescribeMacro.describe

object ChoiceInteractionTransformer extends InteractionTransformer with NamespaceStripper {

  private val logger = LoggerFactory.getLogger(ChoiceInteractionTransformer.this.getClass)

  private def updateChoiceLabels(json:JsObject) = {
    val updatedChoices = JsArray(
      (json \ "model" \ "choices").as[Seq[JsObject]].map { choice =>
        val label = (choice \ "label").as[String]
        choice ++ obj( "label" -> stripNamespaces(label) )
      })

    logger.trace(describe(updatedChoices))

    val out = json.deepMerge(
      obj("model" ->
        obj("choices" -> updatedChoices)))

    logger.trace(describe(out))
    out
  }

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = {
    val transformed = SuperChoiceInteractionTransformer.interactionJs(qti, manifest)
    val out = transformed.map { case (id, json) => id -> updateChoiceLabels(json) }
    logger.trace(describe(out))
    out
  }

  override def transform(node: Node, manifest: Node): Seq[Node] =
    SuperChoiceInteractionTransformer.transform(node, manifest)
  
}
