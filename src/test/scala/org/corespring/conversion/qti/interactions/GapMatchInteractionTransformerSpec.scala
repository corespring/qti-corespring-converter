package org.corespring.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json._

import scala.collection.immutable.TreeMap
import scala.xml.XML

class GapMatchInteractionTransformerSpec extends Specification {

  val responseIdentifier = "RESPONSE"

  val categories = List("t1", "t2", "t3", "t4", "t5")
  val choices = TreeMap("s1" -> "630", "s2" -> "24", "s3" -> "80", "s4" -> "90", "s5" -> "300", "s6" -> "900",
    "s7" -> "30", "s8" -> "800", "s9" -> "240", "s10" -> "63")

  val gapMatchInteraction = <gapMatchInteraction responseIdentifier={responseIdentifier}>
    {
      choices.map{ case(identifier, label) => <gapText identifier={identifier} matchMax="1">{label}</gapText> }
    }
    <p class="target_paragraph">
      <math display="inline">
        <mrow>
          <mn>80</mn>
          <mo>&#xD7;</mo>
          <mn>3</mn>
          <mo>=</mo>
        </mrow>
      </math>
      <gap class="target" identifier={categories(0)}/>
    </p>
    <p class="target_paragraph">
      <math display="inline">
        <mrow>
          <mn>30</mn>
          <mo>&#xD7;</mo>
          <mn>3</mn>
          <mo>=</mo>
        </mrow>
      </math>
      <gap class="target" identifier={categories(1)}/>
    </p>
    <p class="target_paragraph">
      <math display="inline">
        <mrow>
          <mn>50</mn>
          <mo>&#xD7;</mo>
          <mn>6</mn>
          <mo>=</mo>
        </mrow>
      </math>
      <gap class="target" identifier={categories(2)}/>
    </p>
    <p class="target_paragraph">
      <math display="inline">
        <mrow>
          <mn>40</mn>
          <mo>&#xD7;</mo>
          <mn>2</mn>
          <mo>=</mo>
        </mrow>
      </math>
      <gap class="target" identifier={categories(3)}/>
    </p>
    <p class="target_paragraph">
      <math display="inline">
        <mrow>
          <mn>90</mn>
          <mo>&#xD7;</mo>
          <mn>7</mn>
          <mo>=</mo>
        </mrow>
      </math>
      <gap class="target" identifier={categories(4)}/>
    </p>
  </gapMatchInteraction>

  val correctResponse = Map("t1" -> "s9", "t2" -> "s4", "t3" -> "s5", "t4" -> "s3", "t5" -> "s1")

  val qti = <assessmentItem>
    <responseDeclaration baseType="directedPair" cardinality="multiple" identifier={responseIdentifier}>
      <correctResponse>
        {
          correctResponse.map{ case(category, choice) =>
            <value>{s"$choice $category"}</value>
          }
        }
      </correctResponse>
      <mapping>
        <mapEntry mapKey="s9 t1" mappedValue="1"/>
        <mapEntry mapKey="s4 t2" mappedValue="1"/>
        <mapEntry mapKey="s5 t3" mappedValue="1"/>
        <mapEntry mapKey="s3 t4" mappedValue="1"/>
        <mapEntry mapKey="s1 t5" mappedValue="1"/>
      </mapping>
    </responseDeclaration>
    <outcomeDeclaration baseType="float" cardinality="single" identifier="SCORE" normalMaximum="1" normalMinimum="0">
      <defaultValue>
        <value>0</value>
      </defaultValue>
    </outcomeDeclaration>
    <itemBody>
      <div class="row">
        <div class="span8">
          <div class="item_stem">
            <p class="stem_paragraph">Drag and drop&#xA0;a number to correctly complete each equation. Not all numbers will be used.</p>
          </div>
          {gapMatchInteraction}
        </div>
      </div>
    </itemBody>
  </assessmentItem>

  "interactionJs" should {

    val result = GapMatchInteractionTransformer.interactionJs(qti, QTIManifest.EmptyManifest).get(responseIdentifier)
      .getOrElse(throw new Exception(s"Result did not contain interaction for $responseIdentifier"))

    println(Json.prettyPrint(result))

    "correctResponse" should {

      val correctResponseJs = (result \ "correctResponse").as[JsObject]

      "map <correctResponse/> values to keyed object" in {
        correctResponse.map{ case (key, value) => {
          (correctResponseJs \ key).as[Seq[String]] must be equalTo(Seq(value))
        }}.last
      }

    }

    "feedback" should {

      val feedback = (result \ "feedback").as[JsObject]

      "be 'none' for all types" in {
        feedback must be equalTo(Json.obj(
          "correctFeedbackType" -> "none",
          "partialFeedbackType" -> "none",
          "incorrectFeedbackType" -> "none"
        ))
      }

    }


    "model" should {

      val model = (result \ "model").as[JsObject]

      "answerAreaXhtml" should {

        val answerAreaXhtml = (model \ "answerAreaXhtml").as[String]

        "not contain any <gapText/> nodes" in {
          answerAreaXhtml must not contain "<gapText"
        }

        "replace <gap/> nodes with <answer-area-inline-csdndi/> nodes, converting identifier to id" in {
          val answerAreaResult = XML.loadString(s"<div>$answerAreaXhtml</div>") \\ "answer-area-inline-csdndi"
          (gapMatchInteraction \\ "gap").zip(answerAreaResult).map{ case(gap, answerArea) => {
            (gap \\ "@identifier").text must be equalTo((answerArea \\ "@id").text)
          }}

          answerAreaXhtml must not contain "<gap "
        }

      }


      "answerAreas" should {

        val answerAreas = (model \ "answerAreas").as[Seq[JsObject]]

        "contain all categories" in {
          categories.map{ category => {
            Json.obj("id" -> category)
          }} must be equalTo(answerAreas.toList)
        }

      }

      "choices" should {

        val choicesJs = (model \ "choices").as[List[JsObject]]

        "contain all choices" in {
          choicesJs.map{ jsChoice => (jsChoice \ "id").as[String] -> (jsChoice \ "label").as[String] }.toMap must be equalTo(choices)
        }

        "have labelType set to 'text'" in {
          choicesJs.map{ jsChoice => (jsChoice \ "labelType").as[String] must be equalTo("text") }.last
        }

      }

      "config" should {

        val config = (model \ "config").as[JsObject]

        "have 'shuffle' = false" in {
          (config \ "shuffle").as[Boolean] must beFalse
        }

        "have an empty choiceAreaLabel" in {
          (config \ "choiceAreaLabel").as[String] must be equalTo("")
        }

        "have choiceAreaLayout = 'horizontal'" in {
          (config \ "choiceAreaLayout").as[String] must be equalTo("horizontal")
        }

        "have choiceAreaPosition = 'above'" in {
          (config \ "choiceAreaPosition").as[String] must be equalTo("above")
        }

        "have removeAllAfterPlacing = true" in {
          (config \ "removeAllAfterPlacing").as[Boolean] must beTrue
        }

      }

    }


  }

}
