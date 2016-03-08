package org.corespring.conversion.qti.processing.transformers

import scala.xml.Node

abstract class ResponseProcessingTransformer {

  def transform(node: Node): Node

}

object ResponseProcessingTransformer {

  val all = Seq(
    FieldValueProcessingTransformer
  )

  def transformAll(node: Node): Node = all.foldLeft(node){ case (acc, transformer) => transformer.transform(acc) }

}