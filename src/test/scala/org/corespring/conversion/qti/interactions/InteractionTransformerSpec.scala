package org.corespring.conversion.qti.interactions

import org.specs2.mutable.Specification
import play.api.libs.json._
import scala.xml._

class InteractionTransformerSpec extends Specification {

  val transformer = new InteractionTransformer {
    override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = ???
    override def transform(node: Node, manifest: Node): Seq[Node] = node
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

  "optForAttr" should {

    implicit val node = <span class="great" count="2" awesome="true" empty="">Test</span>

    "return Some[JsString] when present" in {
      transformer.optForAttr[JsString]("class") must be equalTo Some(JsString("great"))
    }

    "return None when blank String" in {
      transformer.optForAttr[JsString]("empty") must beNone
    }

    "return Some[JsNumber] when present" in {
      transformer.optForAttr[JsNumber]("count") must be equalTo Some(JsNumber(2))
    }

    "return Some[JsBoolean] when present" in {
      transformer.optForAttr[JsBoolean]("awesome") must be equalTo Some(JsBoolean(true))
    }

    "return None when not present" in {
      transformer.optForAttr[JsString]("id") must beNone
    }
  }

  "withPrompt" should {

    val prompt = "Hey, this is a prompt!"
    val sourceWithPrompt = <interaction><prompt>{ prompt }</prompt></interaction>
    val sourceWithoutPrompt = <interaction></interaction>
    val result = <corespring-interaction></corespring-interaction>

    "add prompt when source contains one" in {
      new InteractionTransformer {
        def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = ???
        (result.withPrompt(sourceWithPrompt) \\ "p").find(n => n.text == prompt) must not beEmpty
        override def transform(node: Node, manifest: Node): Seq[Node] = node
      }
      success
    }

    "add nothing when source contains no prompt" in {
      new InteractionTransformer {
        def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = ???
        (result.withPrompt(sourceWithoutPrompt) \\ "p").find(n => n.text == prompt) must beEmpty
        override def transform(node: Node, manifest: Node): Seq[Node] = node
      }
      success
    }

  }

}