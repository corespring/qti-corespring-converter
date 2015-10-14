package com.keydatasys.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json._

import scala.xml._

case class SelectPointInteractionTransformer(qti: Node) extends InteractionTransformer {

  private object InteractionType extends Enumeration {
    type InteractionType = Value
    val Points, Line, MultiLine, Unknown = Value
  }

  import InteractionType._

  /**
   * Determine whether the QTI interaction provided by KDS is intended to be a line interaction or a set of points.
   */
  private def getType(node: Node): InteractionType = {

    /**
     * Return true if the provided nodes represent a set of points, false otherwise. In the case of KDS, this means
     * that each Node must be a <value/> with defined xcoordinate and ycoordinate attributes.
     */
    def valuesArePoints(values: Seq[Node]) = {
      def valueIsPoint(value: Node) = value.label == "value" &&
        (value \ "@xcoordinate").nonEmpty && (value \ "@ycoordinate").nonEmpty
      values.find(value => !valueIsPoint(value)).isEmpty
    }

    /**
     * Return true if the provided nodes represents a single <value/> with the required attributes defined.
     */
    def lineCount(values: Seq[Node]): Int = {
      val requiredAttributes = Seq("startPointXCoordinate", "startPointYCoordinate", "startPointConsider",
        "startPointTolerance", "endPointXCoordinate", "endPointYCoordinate", "endPointConsider", "endPointTolerance",
        "xIntercept", "xInterceptConsider", "xInterceptTolerance", "yIntercept", "yInterceptConsider",
        "yInterceptTolerance", "slope", "slopeConsider", "slopeTolerance")
      def hasRequiredAttributes(node: Node) =
        requiredAttributes.find(attribute => node.attribute(attribute).isEmpty).isEmpty
      values.filter(hasRequiredAttributes(_)).length
    }

    val values = (responseDeclaration(node, qti) \ "correctResponse" \ "value").toSeq

    values match {
      case _ if (valuesArePoints(values)) => Points
      case _ if (lineCount(values) == 1) => Line
      case _ if (lineCount(values) > 1) => MultiLine
      case _ => Unknown
    }

  }

  private def model(implicit node: Node) = {
    val hasDecimal = "(\\d+)\\.(\\d+)".r
    def propertyNumber(string: String): JsNumber = {
      val isZero = "0+".r
      string match {
        case hasDecimal(one, decimal) => decimal match {
          case isZero() => JsNumber(one.toInt)
          case _ => JsNumber(BigDecimal(string))
        }
        case _ => JsNumber(string.toInt)
      }
    }

    def largest(one: Option[String], two: Option[String]): Option[String] = {
      ((one, two) match {
        case (Some(a), None) => Some(a)
        case (None, Some(b)) => Some(b)
        case (Some(a), Some(b)) => a.toFloat.abs.compare(b.toFloat.abs) match {
          case x: Int if x > 0 => Some(a)
          case _ => Some(b)
        }
        case _ => one
      }).map(_.replaceAll("-", ""))
    }

    Json.obj(
      "config" -> partialObj(
        "domainLabel" -> property("xAxisTitle").map(JsString(_)),
        "rangeLabel" -> property("yAxisTitle").map(JsString(_)),
        "graphWidth" -> Some(JsNumber(550)),
        "graphHeight" -> Some(JsNumber(550)),
        "graphPadding" -> Some(JsNumber(50)),
        "domainMin" -> property("xAxisMinValue").map(propertyNumber),
        "domainMax" -> property("xAxisMaxValue").map(propertyNumber),
        "domainStepValue" -> property("xAxisStepValue").map(propertyNumber),
        "domainLabelFrequency" -> property("xAxisLabelPattern").map(propertyNumber),
        "rangeMin" -> property("yAxisMinValue").map(propertyNumber),
        "rangeMax" -> property("yAxisMaxValue").map(propertyNumber),
        "rangeStepValue" -> property("yAxisStepValue").map(propertyNumber),
        "rangeLabelFrequency" -> property("yAxisLabelPattern").map(propertyNumber),
        "scale" -> property("xAxisStepValue").map(propertyNumber),
        "tickLabelFrequency" -> property("xAxisLabelPattern").map(propertyNumber),
        "showCoordinates" -> Some(JsBoolean(false)),
        "maxPoints" -> ((node \ "@maxChoices") match {
          case n: NodeSeq if n.nonEmpty => Some(JsNumber(n.text.toInt))
          case _ => None
        })))
  }

  private def property(name: String)(implicit node: Node): Option[String] =
    (node \ "object" \ "param").toSeq.find(p => (p \ "@name").text == name).map(p => (p \ "@value").text)

  override def transform(node: Node, manifest: Node) = node.label match {
    case "selectPointInteraction" => getType(node) match {
      case Points => PointsInteractionTransformer.transform(node, manifest)
      case Line => LineInteractionTransformer.transform(node, manifest)
      case MultiLine => {
        println(s"${qti \ "@identifier"} - No support for multi-line interactions")
        node
      }
      case _ => throw new IllegalArgumentException(s"$node does not represent any known KDS format")
    }
    case _ => node
  }

  override def interactionJs(node: Node, manifest: Node) =
    PointsInteractionTransformer.interactionJs(node, manifest) ++
      LineInteractionTransformer.interactionJs(node, manifest)

  private object LineInteractionTransformer extends InteractionTransformer {

    override def transform(node: Node, manifest: Node) = {
      val identifier = (node \ "@responseIdentifier").text
      node match {
        case elem: Elem if (node.label == "selectPointInteraction") =>
          elem.child.filter(_.label != "object").map(n => n.label match {
            case "prompt" => <p class="prompt">{ n.child }</p>
            case _ => n
          }) ++ <corespring-line id={ identifier }></corespring-line>
        case _ => node
      }
    }

    override def interactionJs(qti: Node, manifest: Node) = (qti \\ "selectPointInteraction").map(implicit node => {
      getType(node) match {
        case Line => {
          Some((node \ "@responseIdentifier").text -> Json.obj(
            "componentType" -> "corespring-line",
            "correctResponse" -> (responseDeclaration(node, qti) \ "correctResponse" \ "value")
              .map(n => s"y=${(n \ "@slope").text}x+${(n \ "@yIntercept").text}").head,
            "model" -> model(node)))
        }
        case _ => None
      }
    }).flatten.toMap

  }

  private object PointsInteractionTransformer extends InteractionTransformer {

    override def transform(node: Node, manifest: Node) = {
      val identifier = (node \ "@responseIdentifier").text
      node match {
        case elem: Elem if (node.label == "selectPointInteraction") =>
          elem.child.filter(_.label != "object").map(n => n.label match {
            case "prompt" => <p class="prompt">{ n.child }</p>
            case _ => n
          }) ++ <corespring-point-intercept id={ identifier }></corespring-point-intercept>
        case _ => node
      }
    }

    override def interactionJs(qti: Node, manifest: Node) = (qti \\ "selectPointInteraction").map(implicit node => {
      getType(node) match {
        case Points => {
          Some((node \ "@responseIdentifier").text -> Json.obj(
            "componentType" -> "corespring-point-intercept",
            "correctResponse" -> answers(qti)(node),
            "model" -> model(node)))
        }
        case _ => None
      }
    }).flatten.toMap

    private def answers(qti: Node)(implicit node: Node) =
      (responseDeclaration(node, qti) \ "correctResponse" \ "value").toSeq
        .map(v => s"${(v \ "@xcoordinate").text},${(v \ "@ycoordinate").text}")

  }

}