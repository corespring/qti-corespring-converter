package com.keydatasys.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.corespring.conversion.qti.transformers._
import org.specs2.mutable.Specification
import play.api.libs.json._

import scala.xml.Node

class SelectPointInteractionTransformerSpec extends Specification {

  "points interaction" should {

    val prompt = "This is a prompt"
    val identifier = "RESPONSE1"
    val maxChoices = 3
    val correctResponses = Seq((2, 1), (5, 3), (8, 5))
    val settings = Map("gridVisible" -> "1", "gridHeightInPixels" -> "600", "gridWidthInPixels" -> "600",
      "xAxisTitle" -> "time", "xAxisMinValue" -> "0", "xAxisMaxValue" -> "8", "xAxisStepValue" -> "1",
      "xAxisLabelPattern" -> "1", "yAxisTitle" -> "tacos", "yAxisMinValue" -> "0", "yAxisMaxValue" -> "8",
      "yAxisStepValue" -> "1", "yAxisLabelPattern" -> "1")

    def getSetting(string: String) = settings.get(string).getOrElse(throw new IllegalStateException(s"No setting for $string"))

    def qti(prompt: String = prompt, identifier: String = identifier, correctResponses: Seq[(Int, Int)] = correctResponses,
            settings: Map[String, String] = settings) =
      <assessmentItem>
        <responseDeclaration identifier={ identifier } cardinality="record">
          <fieldValue identifier="xcoordinate" baseType="float"/>
          <fieldValue identifier="ycoordinate" baseType="float"/>
          <correctResponse>
            {
            correctResponses.map {
              case (x, y) =>
                  <value xcoordinate={ x.toString } ycoordinate={ y.toString } tolerance="0"/>
            }
            }
          </correctResponse>
        </responseDeclaration>
        <itemBody>
          <selectPointInteraction responseIdentifier={ identifier } minChoices="0" maxChoices={ maxChoices.toString } class="placePointsSelectPointInteraction">
            <prompt visible="true">{ prompt }</prompt>
            <object>
              {
              settings.map {
                case (name, value) =>
                    <param name={ name } valuetype="DATA" value={ value }/>
              }
              }
            </object>
          </selectPointInteraction>
        </itemBody>
      </assessmentItem>

    def transformer(qti: Node) = new SelectPointInteractionTransformer(qti)

    def output(qtiNode: Node = qti()) = new InteractionRuleTransformer(transformer(qtiNode)).transform(qtiNode)
    def jsonOutput(qtiNode: Node = qti(), identifier: String = identifier): JsObject = {
      transformer(qtiNode).interactionJs(qtiNode, QTIManifest.EmptyManifest).get(identifier)
        .getOrElse(throw new IllegalStateException(s"Missing JSON for $identifier"))
    }

    "transform" should {

      "return <corespring-point-intercept/> node" in {
        (output() \\ "corespring-point-intercept").head must be equalTo (<corespring-point-intercept id={ identifier }/>)
      }

      "return <p class='prompt'/> containing prompt" in {
        (output() \\ "p").find(p => (p \ "@class").text.contains("prompt"))
          .getOrElse(throw new IllegalStateException("Contained no prompt!")).text must be equalTo (prompt)
      }

      "not return <object/>" in {
        (output() \\ "object").isEmpty must beTrue
      }

    }

    "interactionJs" should {

      "contain componentType = corespring-point-intercept" in {
        (jsonOutput() \ "componentType").as[String] must be equalTo ("corespring-point-intercept")
      }

      "contain correctResponse" in {
        (jsonOutput() \ "correctResponse").as[Seq[String]] must be equalTo (correctResponses.map { case (x, y) => s"$x,$y" })
      }

      "contain domainLabel" in {
        (jsonOutput() \ "model" \ "config" \ "domainLabel").as[String] must be equalTo (getSetting("xAxisTitle"))
      }

      "contain rangeLabel" in {
        (jsonOutput() \ "model" \ "config" \ "rangeLabel").as[String] must be equalTo (getSetting("yAxisTitle"))
      }

      "contain graphWidth" in {
        (jsonOutput() \ "model" \ "config" \ "graphWidth").as[Int] must be equalTo (550)
      }

      "contain graphHeight" in {
        (jsonOutput() \ "model" \ "config" \ "graphHeight").as[Int] must be equalTo (550)
      }

      "contain domainMin" in {
        (jsonOutput() \ "model" \ "config" \ "domainMin").as[Int] must be equalTo (getSetting("xAxisMinValue").toInt)
      }

      "contain domainMax" in {
        (jsonOutput() \ "model" \ "config" \ "domainMax").as[Int] must be equalTo (getSetting("xAxisMaxValue").toInt)
      }

      "contain domainStepValue" in {
        (jsonOutput() \ "model" \ "config" \ "domainStepValue").as[Int] must be equalTo (getSetting("xAxisStepValue").toInt)
      }

      "contain domainLabelFrequency" in {
        (jsonOutput() \ "model" \ "config" \ "domainLabelFrequency").as[Int] must be equalTo (getSetting("xAxisLabelPattern").toInt)
      }

      "contain rangeMin" in {
        (jsonOutput() \ "model" \ "config" \ "rangeMin").as[Int] must be equalTo (getSetting("yAxisMinValue").toInt)
      }

      "contain rangeMax" in {
        (jsonOutput() \ "model" \ "config" \ "rangeMax").as[Int] must be equalTo (getSetting("yAxisMaxValue").toInt)
      }

      "contain rangeStepValue" in {
        (jsonOutput() \ "model" \ "config" \ "rangeStepValue").as[Int] must be equalTo (getSetting("yAxisStepValue").toInt)
      }

      "contain rangeLabelFrequency" in {
        (jsonOutput() \ "model" \ "config" \ "rangeLabelFrequency").as[Int] must be equalTo (getSetting("yAxisLabelPattern").toInt)
      }

      "contain maxPoints" in {
        (jsonOutput() \ "model" \ "config" \ "maxPoints").as[Int] must be equalTo (maxChoices)
      }

      "contain graphPadding" in {
        (jsonOutput() \ "model" \ "config" \ "graphPadding").as[Int] must be equalTo (50)
      }

      "contain domainGraphPadding" in {
        (jsonOutput() \ "model" \ "config" \ "domainGraphPadding").as[Int] must be equalTo (SelectPointInteractionTransformer.Defaults.domainGraphPadding.toInt)
      }

      "contain rangeGraphPadding" in {
        (jsonOutput() \ "model" \ "config" \ "rangeGraphPadding").as[Int] must be equalTo (SelectPointInteractionTransformer.Defaults.rangeGraphPadding.toInt)
      }

    }

  }

  "line interaction" should {

    val identifier = "RESPONSE1"
    val prompt = "This is the prompt"
    val settings = Map("gridVisible" -> "1", "gridHeightInPixels" -> "300", "gridWidthInPixels" -> "300",
      "xAxisTitle" -> "time", "xAxisMinValue" -> "0", "xAxisMaxValue" -> "8", "xAxisStepValue" -> "1",
      "xAxisLabelPattern" -> "1", "yAxisTitle" -> "tacos", "yAxisMinValue" -> "0", "yAxisMaxValue" -> "160",
      "yAxisStepValue" -> "20", "yAxisLabelPattern" -> "1")

    val b = "0"
    val m = "10"

    def getSetting(string: String) = settings.get(string).getOrElse(throw new IllegalStateException(s"No setting for $string"))

    def qti(prompt: String = prompt, identifier: String = identifier, settings: Map[String, String] = settings,
            m: String = m, b: String = b) =
      <assessmentItem xmlns="http://www.imsglobal.org/xsd/imsqti_v2p1" xmlns:m="http://www.w3.org/1998/Math/MathML" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.imsglobal.org/xsd/imsqti_v2p1 http://www.imsglobal.org/xsd/imsqti_v2p1.xsd" title="663885" identifier="663885" adaptive="false" timeDependent="false">
        <responseDeclaration identifier={ identifier } cardinality="record">
          <fieldValue identifier="startPointXCoordinate" baseType="float"/>
          <fieldValue identifier="startPointYCoordinate" baseType="float"/>
          <fieldValue identifier="endPointXCoordinate" baseType="float"/>
          <fieldValue identifier="endPointYCoordinate" baseType="float"/>
          <fieldValue identifier="XIntercept" baseType="float"/>
          <fieldValue identifier="YIntercept" baseType="float"/>
          <fieldValue identifier="Slope" baseType="float"/>
          <correctResponse>
            <value startPointXCoordinate="0" startPointYCoordinate="0" startPointConsider="1" startPointTolerance="0" endPointXCoordinate="8" endPointYCoordinate="80" endPointConsider="0" endPointTolerance="0" xIntercept="0" xInterceptConsider="0" xInterceptTolerance="0" yIntercept={ b } yInterceptConsider="0" yInterceptTolerance="0" slope={ m } slopeConsider="1" slopeTolerance="0.25"/>
          </correctResponse>
        </responseDeclaration>
        <outcomeDeclaration identifier="SCORE" cardinality="single" baseType="float">
          <defaultValue>
            <value>0</value>
          </defaultValue>
        </outcomeDeclaration>
        <itemBody>
          <selectPointInteraction responseIdentifier={ identifier } minChoices="0" maxChoices="1" class="singleLineSelectPointInteraction">
            <prompt visible="true">{ prompt }</prompt>
            <object>
              {
              settings.map {
                case (name, value) =>
                    <param name={ name } valuetype="DATA" value={ value }/>
              }
              }
            </object>
          </selectPointInteraction>
        </itemBody>
      </assessmentItem>

    def transformer(qti: Node) = new SelectPointInteractionTransformer(qti)

    def output(qtiNode: Node = qti()) = new InteractionRuleTransformer(transformer(qtiNode)).transform(qtiNode)
    def jsonOutput(qtiNode: Node = qti(), identifier: String = identifier): JsObject = {
      transformer(qtiNode).interactionJs(qtiNode, QTIManifest.EmptyManifest).get(identifier)
        .getOrElse(throw new IllegalStateException(s"Missing JSON for $identifier"))
    }

    "transform" should {

      "return <corespring-line/> node" in {
        (output() \\ "corespring-line").head must be equalTo (<corespring-line id={ identifier }/>)
      }

      "return <p class='prompt'/> containing prompt" in {
        (output() \\ "p").find(p => (p \ "@class").text.contains("prompt"))
          .getOrElse(throw new IllegalStateException("Contained no prompt!")).text must be equalTo (prompt)
      }

      "not return <object/>" in {
        (output() \\ "object").isEmpty must beTrue
      }

      "transform the prompt" in {
        val node = BadPrompt.QTI
        val t = new SelectPointInteractionTransformer(BadPrompt.QTI)
        val out = t.transform( (node \\ "selectPointInteraction")(0))
        println(s"out: $out")
        true must_== false
      }

    }

    "interactionJS" should {

      "contain componentType = corespring-line" in {
        (jsonOutput() \ "componentType").as[String] must be equalTo ("corespring-line")
      }

      "contain domainLabel" in {
        (jsonOutput() \ "model" \ "config" \ "domainLabel").as[String] must be equalTo (getSetting("xAxisTitle"))
      }

      "contain rangeLabel" in {
        (jsonOutput() \ "model" \ "config" \ "rangeLabel").as[String] must be equalTo (getSetting("yAxisTitle"))
      }

      "contain graphWidth" in {
        (jsonOutput() \ "model" \ "config" \ "graphWidth").as[Int] must be equalTo (550)
      }

      "contain graphHeight" in {
        (jsonOutput() \ "model" \ "config" \ "graphHeight").as[Int] must be equalTo (550)
      }

      "contain domainMin" in {
        (jsonOutput() \ "model" \ "config" \ "domainMin").as[Int] must be equalTo (getSetting("xAxisMinValue").toInt)
      }

      "contain domainMax" in {
        (jsonOutput() \ "model" \ "config" \ "domainMax").as[Int] must be equalTo (getSetting("xAxisMaxValue").toInt)
      }

      "contain domainStepValue" in {
        (jsonOutput() \ "model" \ "config" \ "domainStepValue").as[Int] must be equalTo (getSetting("xAxisStepValue").toInt)
      }

      "contain domainLabelFrequency" in {
        (jsonOutput() \ "model" \ "config" \ "domainLabelFrequency").as[Int] must be equalTo (getSetting("xAxisLabelPattern").toInt)
      }

      "contain rangeMin" in {
        (jsonOutput() \ "model" \ "config" \ "rangeMin").as[Int] must be equalTo (getSetting("yAxisMinValue").toInt)
      }

      "contain rangeMax" in {
        (jsonOutput() \ "model" \ "config" \ "rangeMax").as[Int] must be equalTo (getSetting("yAxisMaxValue").toInt)
      }

      "contain rangeStepValue" in {
        (jsonOutput() \ "model" \ "config" \ "rangeStepValue").as[Int] must be equalTo (getSetting("yAxisStepValue").toInt)
      }

      "contain rangeLabelFrequency" in {
        (jsonOutput() \ "model" \ "config" \ "rangeLabelFrequency").as[Int] must be equalTo (getSetting("yAxisLabelPattern").toInt)
      }

      "contain graphPadding" in {
        (jsonOutput() \ "model" \ "config" \ "graphPadding").as[Int] must be equalTo (50)
      }

      "contain correctResponse in y=mx+b format" in {
        (jsonOutput() \ "correctResponse").as[String] must be equalTo (s"y=${m}x+$b")
      }
      "contain domainGraphPadding" in {
        (jsonOutput() \ "model" \ "config" \ "domainGraphPadding").as[Int] must be equalTo (SelectPointInteractionTransformer.Defaults.domainGraphPadding.toInt)
      }

      "contain rangeGraphPadding" in {
        (jsonOutput() \ "model" \ "config" \ "rangeGraphPadding").as[Int] must be equalTo (SelectPointInteractionTransformer.Defaults.rangeGraphPadding.toInt)
      }

    }

  }

}