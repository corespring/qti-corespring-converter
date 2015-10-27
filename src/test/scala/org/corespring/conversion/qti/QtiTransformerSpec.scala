package org.corespring.conversion.qti

import org.specs2.mutable.Specification
import play.api.libs.json.JsObject

import scala.xml.XML

class QtiTransformerSpec extends Specification {

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

     "transform bad qti found on qa" in {
       val json = QtiTransformer.transform(qti)
       (json \ "components").asOpt[JsObject].isDefined === true
     }

   }


 }

}
