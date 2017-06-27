package org.corespring.conversion.qti

import org.apache.commons.io.IOUtils
import org.corespring.common.file.SourceWrapper
import org.specs2.mutable.Specification
import play.api.libs.json.JsObject

import scala.xml.{Elem, Node, XML}

class QtiTransformerSpec extends Specification {

  def qti(body: Elem) =
    <assessmentItem>
      <responseDeclaration identifier="Q_01" cardinality="single" baseType="string">
        <correctResponse>
          <value>397.66</value>
          <value>397.7</value>
        </correctResponse>
      </responseDeclaration>
      <itemBody> {body} </itemBody>
    </assessmentItem>

 "transform" should {

   "with a textEntryInteraction" should {

     val qti =
       <assessmentItem>
         <responseDeclaration identifier="Q_01" cardinality="single" baseType="string">
           <correctResponse>
             <value>397.66</value>
             <value>397.7</value>
           </correctResponse>
         </responseDeclaration>
         <itemBody>
           <textEntryInteraction responseIdentifier="Q_01" expectedLength="10"/>
         </itemBody>
       </assessmentItem>

     "not throw an exception" in {
       QtiTransformer.transform(qti) must not(throwAn[Exception])
     }

   }

   "with sources" should {
     "convert stylesheets" in {

       val qtiData = qti(<stylesheet href="styles/LiveInspect.css"></stylesheet>)
       val sources: Map[String,SourceWrapper] = Map("styles/LiveInspect.css" ->
         SourceWrapper( "styles/LiveInspect.css", IOUtils.toInputStream("body{color: red;}", "UTF-8")
       ))
       val manifest : Node = MockManifest.manifest

       val json = QtiTransformer.transform(qtiData, sources, manifest)

       val xml = XML.loadString( (json \ "xhtml").as[String] )
       (xml \ "style").head.text must_== """.qti.kds body { color:red; }"""
     }
   }

 }

}
