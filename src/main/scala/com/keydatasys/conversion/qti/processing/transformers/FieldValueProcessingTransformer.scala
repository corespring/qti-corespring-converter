package com.keydatasys.conversion.qti.processing.transformers

import scala.xml.Node
import scala.xml.transform._

object FieldValueProcessingTransformer extends ResponseProcessingTransformer {

  import scala.xml.Utility.trim

  override def transform(node: Node) = {
    new RuleTransformer(new RewriteRule {
      override def transform(node: Node) = (node \ "fieldValue").nonEmpty match {
        case true => {
          <equal>
            <variable identifier={(node \ "fieldValue" \ "variable" \ "@identifier").text}/>
            <correct identifier={(node \ "fieldValue" \ "variable" \ "@identifier").text}/>
          </equal>
        }
        case _ => reduceDuplicates(node)
      }
    }).transform(node).head
  }

  private def reduceDuplicates(node: Node) = {
    Seq("and", "or").contains(node.label) match {
      case true => node.child match {
        case Seq(left, right) if trim(left) == trim(right) => left
        case _ => node
      }
      case _ => node
    }
  }

}
