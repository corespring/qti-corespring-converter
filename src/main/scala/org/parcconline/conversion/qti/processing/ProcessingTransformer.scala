package org.parcconline.conversion.qti.processing

import org.corespring.conversion.qti.processing.{ProcessingTransformer => CoreSpringProcessingTransformer}

import scala.xml.Node

trait ProcessingTransformer extends CoreSpringProcessingTransformer {

  object CustomOperator {
    val StringToNumber = "qti.customOperators.text.StringToNumber"
    val CsvToOrdered = "qti.customOperators.CsvToOrdered"
  }

  override def expression(node: Node)(implicit qti: Node): String = node.label match {
    case "customOperator" => customOperator(node)
    case _ => super.expression(node)
  }

  def customOperator(node: Node)(implicit qti: Node): String = {
    import CustomOperator._
    (node \ "@class").text match {
      case StringToNumber => stringToNumber(node)
      case CsvToOrdered => csvToOrdered(node)
      case _ => throw new UnsupportedCustomOperatorException((node \ "@class").text, id)
    }
  }

  def stringToNumber(node: Node)(implicit qti: Node): String = {
    s"parseInt(${expression(node.withoutEmptyChildren.head)}, 10)"
  }

  def csvToOrdered(node: Node)(implicit qti: Node): String = {
    node.withoutEmptyChildren match {
      case Seq(child) => s"${expression(child)}.split(',')"
      case _ => throw new Exception("Nope")
    }
  }

}
