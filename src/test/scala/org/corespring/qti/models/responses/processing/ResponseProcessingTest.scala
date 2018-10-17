package org.corespring.qti.models.responses.processing

import org.corespring.qti.models.{QtiItem, ResponseDeclaration}
import org.corespring.qti.models.responses.{Response, ArrayResponse, StringResponse}
import org.specs2.mutable.Specification

class ResponseProcessingTest extends Specification {


  "foo" should {
    "work" in {

      val node = scala.xml.XML.load(this.getClass.getResource("/qti/664014/664014.xml"))
      val qti = QtiItem(node)
      println(qti)

      //      val correctResponse : Map[String,Any] = qti.responseDeclarations.foldLeft(Map[String,Any]())( (acc: Map[String,Any], rd: ResponseDeclaration  ) => {
      //        rd.correctResponse.map( cr => {
      ////          acc + ( rd.identifier -> cr.getValues.head )
      //         acc
      //        }).getOrElse(acc)
      //      })

      val variables = Map[String,Any]("NUMCORRECT"-> 0, "SCORE" -> 0)
      val cr : Seq[Response] = Seq( StringResponse("RESPONSE11",  """\frac{\sqrt5}3"""),
        ArrayResponse("RESPONSE2", Seq("1","4")),
        StringResponse("RESPONSE31", "0.75"),
        StringResponse("RESPONSE4", "4")
      )
      val result = qti.responseProcessing.get.process(Some(variables), Some(cr))
      println(s" result: $result")
      true === true
      //      ResponseProcessing(itemBody, type: None)
    }
  }
}
