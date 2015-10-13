package org.corespring.conversion.qti.interactions

import org.specs2.mutable.Specification
import play.api.libs.json.JsObject

import scala.xml.Node

class InteractionTransformerSpec extends Specification {

  val transformer = new InteractionTransformer {
    def interactionJs(qti: Node): Map[String, JsObject] = ???
  }

  val responseIdentifier = "Q_01"

  val responseDeclarationNode = <responseDeclaration identifier={ responseIdentifier }/>

  val interaction = <node responseIdentifier={ responseIdentifier }/>
  val interactionWithNoIdentifier = <node/>
  val interactionWithoutResponseDeclaration = <node responseIdentifier="not-found"/>

  val qti =
    <assessmentItem>
      { responseDeclarationNode }
      <itemBody>
        { Seq(interaction, interactionWithNoIdentifier, interactionWithoutResponseDeclaration) }
      </itemBody>
    </assessmentItem>


  "responseDeclaration" should {

    "return <responseDeclaration/> corresponding to identifier" in {
      transformer.responseDeclaration(interaction, qti) must be equalTo responseDeclarationNode
    }

    "throw IllegalArgumentException when there is no identifier" in {
      transformer.responseDeclaration(interactionWithNoIdentifier, qti) must throwAn[IllegalArgumentException]
    }

    "throw IllegalArgumentException when there is no corresponding <responseDeclaration/>" in {
      transformer.responseDeclaration(interactionWithoutResponseDeclaration, qti) must throwAn[IllegalArgumentException]
    }

  }

  "withPrompt" should {

    val prompt = "Hey, this is a prompt!"
    val sourceWithPrompt = <interaction><prompt>{ prompt }</prompt></interaction>
    val sourceWithoutPrompt = <interaction></interaction>
    val result = <corespring-interaction></corespring-interaction>

    "add prompt when source contains one" in {
      new InteractionTransformer {
        def interactionJs(qti: Node): Map[String, JsObject] = ???
        (result.withPrompt(sourceWithPrompt) \\ "p").find(n => n.text == prompt) must not beEmpty
      }
      success
    }

    "add nothing when source contains no prompt" in {
      new InteractionTransformer {
        def interactionJs(qti: Node): Map[String, JsObject] = ???
        (result.withPrompt(sourceWithoutPrompt) \\ "p").find(n => n.text == prompt) must beEmpty
      }
      success
    }

  }

}
