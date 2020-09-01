package org.corespring.conversion.qti.interactions

import org.apache.commons.text.StringEscapeUtils.unescapeHtml4
import org.corespring.conversion.qti.manifest.QTIManifest
import org.corespring.conversion.qti.transformers._
import org.specs2.execute.Result
import org.specs2.mutable.Specification
import play.api.libs.json._

import scala.xml._
import scala.xml.transform._

class OrderInteractionTransformerTest extends Specification {

  val identifier = "Q_01"
  val shuffle = "true"
  val prompt = "This is my prompt!"
  val answerAreaLabel = "Place answers here"
  val feedbackValue = "Feedback!"

  def qti(correctResponse: Seq[String], csOrderingType: Option[String] = None): Node = {
    val xml = <assessmentItem>
      <responseDeclaration identifier={ identifier } cardinality="ordered" baseType="identifier">
        <correctResponse>{ correctResponse.map(r => <value>{ r }</value>) }</correctResponse>
      </responseDeclaration>
      <itemBody>
        <orderInteraction responseIdentifier={ identifier } shuffle={ shuffle }>
          <prompt>{ prompt }</prompt>
          {
          correctResponse.map(r =>
            <simpleChoice identifier={ r } fixed="true">
              { r }
              <feedbackInline identifier={ r }>{ feedbackValue }</feedbackInline>
            </simpleChoice>)
          }
        </orderInteraction>
      </itemBody>
    </assessmentItem>

    new RuleTransformer(new RewriteRule {
      override def transform(n: Node): NodeSeq = {
        (n, csOrderingType) match {
          case (e: Elem, Some(orderingType)) if (e.label == "orderInteraction") =>
            (e % new UnprefixedAttribute("csOrderingType", orderingType, Null)) % new UnprefixedAttribute("orientation", "horizontal", Null)
          case _ => n
        }
      }
    }).transform(xml).head

  }

  "OrderInteractionTransformer" should {

    val responses = List("a", <img src="puppies.png"/>.toString, "c")
    val transformer = new OrderInteractionTransformer()

    val input = qti(responses)
    val componentsJson = transformer.interactionJs(input, QTIManifest.EmptyManifest)
    val output = new InteractionRuleTransformer(transformer).transform(input)

    val placementInput = qti(responses, Some("placement"))
    val placementComponentsJson = transformer
      .interactionJs(placementInput, QTIManifest.EmptyManifest)
    val placementOutput = new InteractionRuleTransformer(transformer).transform(placementInput)

    val interactionResult =
      componentsJson.get(identifier).getOrElse(throw new RuntimeException(s"No component called $identifier"))

    val placementInteractionResult =
      placementComponentsJson.get(identifier).getOrElse(throw new RuntimeException(s"No component called $identifier"))

    "result must contain <corespring-ordering/> if not placement ordering" in {
      (output \\ "corespring-ordering").find(n => (n \ "@id").text == identifier) must not beEmpty
      val placementType = (interactionResult \ "model" \ "config" \ "placementType").as[JsString]
      placementType === JsString("inPlace")
    }

    "result must contain <corespring-ordering/> if placement ordering" in {
      (placementOutput \\ "corespring-ordering").find(n => (n \ "@id").text == identifier) must not beEmpty
      val placementType = (placementInteractionResult \ "model" \ "config" \ "placementType").as[JsString]
      placementType === JsString("placement")
    }

    "result must contain model.config.choiceAreaLayout = 'horizontal'" in {
      val o : Result =placementComponentsJson.get(identifier) match {
        case Some(json) => (json \ "model" \ "config" \ "choiceAreaLayout").as[String] === "horizontal"
        case _ => failure("No json for identifier")
      }
      o
    }

    "result must contain model.config.answerAreaLabel " in {
      val o : Result = placementComponentsJson.get(identifier) match {
        case Some(json) => (json \ "model" \ "config" \ "answerAreaLabel").as[String] === answerAreaLabel
        case _ => failure("No json for identifier")
      }
      o
    }

    "must contain appropriate shuffle property" in {
      (interactionResult \ "model" \ "config" \ "shuffle").as[Boolean] must be equalTo shuffle.toBoolean
    }

    "must contain the appropriate prompt" in {
      (output \\ "p").find(n => n.text == prompt) must not beEmpty
    }

    "return the correct component type" in {
      (interactionResult \ "componentType").as[String] must be equalTo "corespring-ordering"
    }

    "return the correct response" in {
      ((interactionResult \ "correctResponse").as[Seq[String]] zip responses).map { case (a, b) => a must be equalTo b }
    }

    "return the choices" in {
      val choices = (interactionResult \ "model" \ "choices").as[Seq[JsObject]]
      choices.map(_ \ "label").map(_.as[String]).map(unescapeHtml4) === responses
//      diff responses must beEmpty
      choices.map(_ \ "value").map(_.as[String]).map(unescapeHtml4) === responses
      //diff responses must beEmpty
      choices.map(_ \ "moveOnDrag").map(_.as[Boolean]).find(moveOnDrag => moveOnDrag == false) === Seq.empty
    }

    "return feedback" in {
      val feedback = (interactionResult \ "model" \ "feedback").as[JsArray].value.map(n => {
        ((n \ "value").as[String] -> (n \ "feedback").as[String])
      }).toMap

      feedback.keys.toSeq === responses
      feedback.values.toSet.toSeq must be equalTo Seq(feedbackValue)
    }

  }

}
