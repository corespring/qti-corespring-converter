package org.corespring.conversion.qti.processing

import org.corespring.conversion.qti.processing.transformers.ResponseProcessingTransformer
import play.api.libs.json.Json

import scala.xml._

trait ProcessingTransformer extends V2JavascriptWrapper {

  private val AssumedResponseTemplateVariable = "RESPONSE"

  val templateConverter = new ResponseProcessingTemplateConverter()
  import templateConverter._

  /**
   * Takes a QTI document, and returns a Javascript representation of its <responseProcessing/> node.
   */
  def toJs(qti: Node): Option[JsResponseProcessing] =
    getResponseNode(qti) match {
      case Some(node) => try {
        Some(JsResponseProcessing(
          vars = outcomeDeclarations(qti),
          responseVars = responseDeclarations(qti),
          lines = node.withoutEmptyChildren.map(n => rootNode(n)(qti))))
      } catch {
        case e: Exception => {
          None
        }
      }
      case _ => None
    }

  protected def id(implicit qti: Node) = (qti \ "@identifier").text

  private def getResponseNode(qti: Node): Option[Node] = {
    (qti \ "responseProcessing").headOption.map(ResponseProcessingTransformer.transformAll(_)) match {
      case Some(responseProcessing) => responseProcessing.hasTemplate match {
        case true => (qti \ "responseDeclaration").length match {
          case 1 => Some(responseProcessing.withTemplate
            .substituting(AssumedResponseTemplateVariable -> (qti \ "responseDeclaration" \ "@identifier").head.text))
          case 0 =>
            throw ProcessingTransformerException("Cannot utilize template without response declarations", qti)
          case _ =>
            throw ProcessingTransformerException(s"Cannot utilize template with multiple response declarations for item ${id(qti)}", qti)
        }
        case false => Some(responseProcessing)
      }
      case _ => None
    }
  }

  protected def responseDeclarations(qti: Node): Seq[String] = (qti \ "responseDeclaration").map(_ \ "@identifier").map(_.text)

  def defaultValue(node: Node) = (node \ "@baseType").text match {
    case "string" => default[String](node, Some(""), t => s""""$t"""")
    case "float" => default[Float](node, Some(0.0f))
    case "integer" => default[Int](node, Some(0))
    case "identifier" => default[String](node, None)
    case other: String => throw ProcessingTransformerException(
      s"Cannot parse $node.label of type $other: $node", node)
  }

  protected def default[T](node: Node, _def: Option[T], fn: (String => String) = t => t.toString): String = {
    (node \ "value").length match {
      case 0 => _def match {
        case Some(default) => fn(default.toString)
        case _ => throw ProcessingTransformerException(s"Cannot have unassigned default for: $node", node)
      }
      case 1 => fn((node \ "value").text)
      case _ => throw ProcessingTransformerException(
        "Cannot have multiple default values for outcomeDeclaration: $node", node)
    }
  }

  protected def outcomeDeclarations(qti: Node): Map[String, String] = {
    (qti \\ "outcomeDeclaration").map { outcomeDeclaration =>
      (outcomeDeclaration \ "@identifier").text -> defaultValue(outcomeDeclaration)
    }.toMap
  }

  protected def rootNode(node: Node)(implicit qti: Node): String = {
    node.label match {
      case "responseCondition" => responseCondition(node)
      case "setOutcomeValue" => setOutcomeValue(node)
      case _ => expression(node)
    }
  }

  protected def responseCondition(node: Node)(implicit qti: Node): String =
    node.withoutEmptyChildren.map(child => child.label match {
      case "responseIf" => responseIf(child)
      case "responseElse" => responseElse(child)
      case "responseElseIf" => responseElseIf(child)
      case _ => throw ProcessingTransformerException(s"Not a supported conditional statement: ${child.label} $id", child)
    }).mkString

  protected def responseIf(node: Node)(implicit qti: Node) =
    conditionalStatement(node, "if $string {", " $string ")

  private def responseElseIf(node: Node)(implicit qti: Node) =
    conditionalStatement(node, " else if $string {", " $string ")

  private def responseElse(node: Node)(implicit qti: Node) =
    s" else { ${node.withoutEmptyChildren.map(responseRule).mkString(" ")} }"

  private def conditionalStatement(node: Node, expressionWrapper: String, responseWrapper: String)(implicit qti: Node) =
    node.withoutEmptyChildren.partition(n => !Seq("responseCondition", "setOutcomeValue").contains(n.label)) match {
      case (expressionNodes, responseRuleNodes) => {
        expressionWrapper.replace("$string", expression(expressionNodes.head)) + (responseRuleNodes.length match {
          case 0 => ""
          case _ => responseWrapper.replace("$string", responseRule(responseRuleNodes.head))
        }) + "}"
      }
      case _ => throw new Exception("Error")
    }

  protected def responseRule(node: Node)(implicit qti: Node) = node.label match {
    case "setOutcomeValue" => setOutcomeValue(node)
    case "responseCondition" => responseCondition(node)
    case _ => throw new Exception(s"Unsupported response rule: ${node.label}\n${qti}")
  }

  protected def setOutcomeValue(node: Node)(implicit qti: Node) = {
    s"""${(node \ "@identifier").text} = ${expression(node.withoutEmptyChildren.head)};"""
  }


  protected def expression(node: Node)(implicit qti: Node): String = node.label match {
    case "match" => s"(${_match(node)})"
    case "and" => s"(${and(node)})"
    case "or" => s"(${or(node)})"
    case "gt" => s"(${gt(node)})"
    case "not" => s"(${not(node)})"
    case "gte" => s"(${gte(node)})"
    case "isNull" => s"(${isNull(node)})"
    case "equal" => s"(${equal(node)})"
    case "sum" => sum(node)
    case "subtract" => s"(${subtract(node)})"
    case "divide" => s"(${divide(node)})"
    case "variable" => (node \ "@identifier").text
    case "correct" => correct(node)
    case "baseValue" => baseValue(node)
    case "mapResponse" => mapResponse(node)
    case "multiple" => multiple(node)
    case "containerSize" => containerSize(node)
    case "contains" => contains(node)
    case "member" => contains(node)
    case "patternMatch" => patternMatch(node)
    case "default" => templateDeclaration(node)
    case _ => throw new Exception(s"Not a supported expression: ${node.label} $id")
  }

  protected def gt(node: Node)(implicit qti: Node) = binaryOp(node, ">")

  protected def not(node: Node)(implicit qti: Node) = preFixOp(node, "!")

  protected def multiple(node: Node)(implicit qti: Node) = s"[${node.withoutEmptyChildren.map(expression).mkString(", ")}]"

  protected def contains(node: Node)(implicit qti: Node) = {
    node.withoutEmptyChildren match {
      case child if (child.length != 2) => throw new Exception(s"contains expression must combine two expressions")
      case child => s"contains(${expression(child.head)}, ${expression(child.last)})"
    }
  }

  protected def equal(node: Node)(implicit qti: Node) = {
    def isArray(string: String) = string.startsWith("[") && string.endsWith("]")
    node.withoutEmptyChildren match {
      case child if (child.length != 2) => throw new Exception(s"equal expression must combine two expressions")
      case child => {
        (child.find(_.label == "variable").map(v => (v \ "@identifier").text),
          child.find(_.label == "correct").map(c => (c \ "@identifier").text)) match {
          case (Some(variable), Some(correct)) if (variable == correct) => s"isCorrect('$variable')"
          case _ => {
            node.withoutEmptyChildren.map(expression) match {
              case Seq(lhs, rhs) if ((isArray(lhs) || isArray(rhs))) =>
                s"_.isEmpty(_.xor($lhs, $rhs))"
              case Seq(lhs, rhs) => s"$lhs === $rhs"
              case e: Seq[String] =>
                throw new Exception(s"Match can only have two children in ${node.withoutEmptyChildren}")
            }
          }
        }
      }
    }
  }
  protected def gte(node: Node)(implicit qti: Node) = binaryOp(node, ">=")
  protected def lt(node: Node)(implicit qti: Node) = binaryOp(node, "<")
  protected def lte(node: Node)(implicit qti: Node) = binaryOp(node, "<=")
  protected def isNull(node: Node)(implicit qti: Node) = postFixOp(node, "== undefined")

  protected def correct(node: Node)(implicit qti: Node) = {
    val rd = (qti \ "responseDeclaration").find(rd => (rd \ "@identifier").text == (node \ "@identifier").text)
      .getOrElse(throw ProcessingTransformerException("Did not contain response declaration matching identifier", node))

    def cardinality(block: String) = {
      (rd \ "@cardinality").text match {
        case "single" => block
        case _ => s"[${block}]"
      }
    }

    cardinality(s"${
      (rd \ "correctResponse" \ "value").map(_.text).map(v => {
        (rd \ "@baseType").text match {
          case "string" => s""""$v""""
          case "identifier" => s""""$v""""
          case "directedPair" => directedPair(v)
          case _ => v
        }
      }).mkString(", ")
    }")
  }

  private def directedPair(value: String)(implicit qti: Node) =
    Json.obj("id" -> value, "matchSet" -> Seq(1, 2, 3))


  protected def baseValue(node: Node) = {
    (node \ "@baseType").text match {
      case "string" => s""""${node.text}""""
      case "directedPair" => s"""["${node.text.split(" ").mkString("""", """")}"]"""
      case _ => node.text
    }
  }

  protected def containerSize(node: Node)(implicit qti: Node) = s"${expression(node.withoutEmptyChildren.head)}.length"

  protected def mapResponse(node: Node) = s"mapResponse('${(node \ "@identifier").text}')"

  protected def templateDeclaration(node: Node)(implicit qti: Node) = {
    val identifier = ((node \ "@identifier").text)
    (qti \\ "templateDeclaration").find(td => (td \ "@identifier").text == identifier) match {
      case Some(td) => defaultValue(td)
      case _ => throw new Exception(s"Cannot process default missing templateDeclaration for id $identifier")
    }
  }

  protected def sum(node: Node)(implicit qti: Node) = node.withoutEmptyChildren.map(expression(_).mkString).mkString(" + ")

  protected def subtract(node: Node)(implicit qti: Node) = node.withoutEmptyChildren.map(expression(_).mkString).mkString(" - ")

  protected def divide(node: Node)(implicit qti: Node) = node.withoutEmptyChildren.map(expression(_).mkString).mkString(" / ")

  protected def _match(node: Node)(implicit qti: Node) = equal(node)

  protected def patternMatch(node: Node)(implicit qti: Node) = {
    val pattern = (node \ "@pattern").text
    s"(${expression(node.withoutEmptyChildren.head)}.match(/$pattern/) != null)"
  }

  protected def and(node: Node)(implicit qti: Node) = binaryOp(node, "&&")
  protected def or(node: Node)(implicit qti: Node) = binaryOp(node, "||")

  private def preFixOp(node: Node, op: String)(implicit qti: Node): String = node.withoutEmptyChildren match {
    case child if (child.length == 1) => s"$op${expression(child.head)}"
    case _ => throw new Exception(s"$op expression must only be used for a single expression")
  }

  private def postFixOp(node: Node, op: String)(implicit qti: Node): String = node.withoutEmptyChildren match {
    case child if (child.length == 1) => s"${expression(child.head)} $op"
    case _ => throw new Exception(s"$op expression must only be used for a single expression")
  }

  private def binaryOp(node: Node, op: String)(implicit qti: Node): String = node.mkString(s" $op ")

  implicit class NodeHelper(node: Node) {
    import Utility._
    def withoutEmptyChildren = trim(node).child.filter { child => !child.isInstanceOf[Text] || !child.text.trim.isEmpty }
  }

  protected case class ProcessingTransformerException(message: String, node: Node) extends Exception {
    override def getMessage = message.replace("$label", node.label).replace("$node", node.toString)
  }

}