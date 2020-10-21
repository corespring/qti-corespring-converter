package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.common.util.XHTMLCleaner
import org.corespring.conversion.qti.interactions.pie.InteractionTransformer
import play.api.libs.json.{JsArray, JsString, Json}
import java.util.UUID.randomUUID

import scala.xml._

object AssociateInteractionTransformer extends InteractionTransformer with XHTMLCleaner {

  override def interactionJs(qti: Node, manifest: Node) = (qti)
    .map(implicit qtinode => {

      val componentId = (qti \ "@identifier").text.trim
      var markupString : String = ""
      var isItembodyPromptAdded = false;
      var answers : JsArray = JsArray.empty
      var prompts : JsArray = JsArray.empty
      val emptyArray = Json.arr()
      def correctResponses(node : Node): Seq[JsString] = {
        val values: Seq[Node] = (responseDeclaration(node, qti) \\ "value").toSeq
        values.map(n => JsString(n.text.trim))
      }
      def prompt(node: Node): String = {
        var itemBodyText ="";
        if(!isItembodyPromptAdded) {
          itemBodyText = (qti \\ "itemBody").map { n => removeNamespaces(n).child.filterNot(e => e.label == "associateInteraction" || e.label == "rubricBlock").mkString.toString().trim }(0)
        }
        isItembodyPromptAdded = true;
        val choiceInteractionText =  node.map { n =>  removeNamespaces(n).child.filterNot(e => e.label == "simpleAssociableChoice").mkString.trim}
        (node \ "prompt").length match {
          case length: Int if length == 1 => itemBodyText +  (removeNamespaces(node) \\ "prompt").map{ n => n.child.mkString.trim }(0)
          case _ => itemBodyText + choiceInteractionText(0).toString()
        }
      }
      def relatedAnswer(node : Node, parentNode: Node ): String = {
        val choiceIdentifier = (node \ "@identifier").text
        var associateIdentifier = ""
        val cr = correctResponses(parentNode)
        for (w <- 0 until cr.size) {
          if(cr(w).value.split(" ")(0) == choiceIdentifier) {
            associateIdentifier = cr(w).value.split(" ")(1)
          }
        }
        associateIdentifier
      }
      def getAnswers(parentNode: Node ): JsArray = {
        val cr = correctResponses(parentNode)
        for (w <- 0 until cr.size) {
          for (i <- 0 until (parentNode \\ "simpleAssociableChoice").size) {
            val choiceIdentifier = ((parentNode \\ "simpleAssociableChoice")(i) \ "@identifier").text
            if(cr(w).value.split(" ")(1) == choiceIdentifier) {
              answers = answers :+ Json.obj(
                "title" -> removeNamespaces((parentNode \\ "simpleAssociableChoice")(i)).child.filterNot(e => e.label == "feedbackInline").mkString.trim,
                "id" -> ((parentNode \\ "simpleAssociableChoice")(i) \ "@identifier").text,
              )
            }
          }
        }
        answers
      }
      def getPrompts(parentNode: Node ): JsArray = {
          for (i <- 0 until (parentNode \\ "simpleAssociableChoice").size) {
            val rAnswer : String = relatedAnswer((parentNode \\ "simpleAssociableChoice")(i),parentNode) //JsString(relatedAnswer((parentNode \\ "simpleAssociableChoice")(i),parentNode))
            if(rAnswer == "") {
              prompts = prompts :+ Json.obj(
                "title" -> removeNamespaces((parentNode \\ "simpleAssociableChoice")(i)).child.filterNot(e => e.label == "feedbackInline").mkString.trim,
                "id" -> ((parentNode \\ "simpleAssociableChoice")(i) \ "@identifier").text
              )
            } else {
                prompts = prompts :+ Json.obj(
                "title" -> removeNamespaces((parentNode \\ "simpleAssociableChoice")(i)).child.filterNot(e => e.label == "feedbackInline").mkString.trim,
                "id" -> ((parentNode \\ "simpleAssociableChoice")(i) \ "@identifier").text,
                "relatedAnswer" -> rAnswer
                )
            }
          }
        prompts
      }
      def markup(): String = {
        (qti \\ "associateInteraction").map{ n =>
          markupString += "<br/><match-list id=\"" + (n \ "@responseIdentifier").text + "\"></match-list>"
        }
        markupString
      }
      val json = Json.obj(
        "id" -> randomUUID,
        "name" -> (qti \ "@identifier").text,
        "collectionIds" -> Json.toJson(List("test").distinct),
        "config" -> Json.obj(
          "markup" ->   markup(),
          "models" -> JsArray((qti \\ "associateInteraction").map { ci =>
            Json.obj(
              "id" -> (ci \ "@responseIdentifier").text,
              "prompt" -> prompt(ci),
              "element" -> JsString("match-list"),
              "answers" -> getAnswers(ci),
              "prompts" -> getPrompts(ci)
            )
          }),
          "elements" -> Json.obj(
            "match-list" -> "@pie-element/match-list@latest"
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
