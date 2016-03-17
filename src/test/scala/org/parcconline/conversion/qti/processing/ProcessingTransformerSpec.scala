package org.parcconline.conversion.qti.processing

import org.specs2.mutable.Specification

class ProcessingTransformer2Spec extends Specification with ProcessingTransformer {

  "customOperator" should {

    implicit val qti = <div></div>

    CustomOperator.StringToNumber should {

      val node = <customOperator class={CustomOperator.StringToNumber}>
        <baseValue baseType="string">test</baseValue>
      </customOperator>

      "return expression wrapped in parseInt" in {
        customOperator(node) must be equalTo(s"""parseInt(${expression(node.withoutEmptyChildren.head)}, 10)""")
      }

    }

    CustomOperator.CsvToOrdered should {

      val node = <customOperator class={CustomOperator.CsvToOrdered}>
        <baseValue baseType="string">1,2,3,4</baseValue>
      </customOperator>

      "return a split on the expression" in {
        customOperator(node) must be equalTo(s"""${expression(node.withoutEmptyChildren.head)}.split(',')""")
      }



      "Unsupported operator" should {

        val itemId = "abc123"
        implicit val qti = <assessmentItem identifier={itemId}></assessmentItem>

        val node = <customOperator class="org.corespring.unsupported.CustomOperator"></customOperator>

        "throw UnsupportedCustomOperatorException" in {
          customOperator(node) must throwA[UnsupportedCustomOperatorException]
        }

      }

    }

  }

}
