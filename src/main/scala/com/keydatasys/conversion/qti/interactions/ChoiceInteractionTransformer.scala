package com.keydatasys.conversion.qti.interactions

import org.corespring.common.util.XHTMLCleaner
import org.corespring.conversion.qti.interactions.InteractionTransformer
import org.corespring.conversion.qti.interactions.{ChoiceInteractionTransformer => CorespringChoiceInteractionTransformer}
import play.api.libs.json._

import scala.xml._
import scala.xml.transform._

object ChoiceInteractionTransformer extends InteractionTransformer with XHTMLCleaner {

  override def transform(node: Node, manifest: Node) = new RuleTransformer(new RewriteRule {
    override def transform(n: Node): NodeSeq = n match {
      case n: Node if (Seq("inlineChoiceRationales", "choiceRationales").contains(n.label)) => Seq.empty
      case n => n
    }
  }).transform(CorespringChoiceInteractionTransformer.transform(node, manifest))

  override def interactionJs(qti: Node, manifest: Node) =
    CorespringChoiceInteractionTransformer.interactionJs(qti, manifest).map {
      case (id, json) => id -> json.deepMerge(partialObj(
        "feedback" -> ((json \ "componentType").asOpt[String] match {
          case Some("corespring-inline-choice") =>
            Some(JsArray((json \ "model" \ "choices").as[Seq[JsObject]].map { choice =>
              Json.obj("value" -> (choice \ "value").as[String], "feedbackType" -> "default")
            }))
          case _ => None
        }),
        "rationales" -> Some(JsArray((json \ "model" \ "choices").as[Seq[JsObject]].map(c => Json.obj(
          "choice" -> Some(c \ "value"),
          "rationale" -> rationale(qti, id, (c \ "value").as[String])
        )))),
        "model" -> Some(Json.obj("shuffle" -> false)))
      )
    }

  private def rationale(qti: Node, id: String, choiceId: String): Option[JsString] =
    (qti \\ "choiceRationales" ++ qti \\ "inlineChoiceRationales").find(c => (c \ "@responseIdentifier").text == id)
      .map(c => (c \\ "rationale").find(r => (r \ "@identifier").text == choiceId)).flatten
      .map(n => JsString(n.child.mkString.cleanWhitespace))

}
