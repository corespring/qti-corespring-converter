package org.corespring.conversion.qti

import org.corespring.conversion.qti.manifest.{CssManifestResource, ManifestItem}
import org.slf4j.LoggerFactory
import org.specs2.mutable.Specification

import scala.xml.{Elem, XML}

class QtiTransformerSpec extends Specification {

  private val logger = LoggerFactory.getLogger(this.getClass)

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

     val mi = ManifestItem("1", "1", Seq.empty, <resource/>)

     "not throw an exception" in {
       QtiTransformer.transform(qti, mi) must not(throwAn[Exception])
     }

   }

   "with sources" should {

     "convert stylesheets if not in itemBody" in {

       val qtiData = <assessmentItem>
         <stylesheet href="style/LiveInspect.css"></stylesheet>
         <itemBody>no style in item body</itemBody>
       </assessmentItem>

       val mi = ManifestItem("1", "1", Seq(
         CssManifestResource("style/LiveInspect.css", "body{color: red}")
       ), <resource/>)

       val json = QtiTransformer.transform(qtiData, mi)

       val xml = XML.loadString( s"<root> ${(json \ "xhtml").as[String]}</root>" )
       logger.debug(s"xml: $xml")
       (xml \\ "style").length must_== 2
       (xml \\ "style")(1).text.trim must_== """/* style/LiveInspect.css */ .qti.kds body { color:red; }"""
     }

     "convert stylesheets" in {

       val qtiData = qti(<stylesheet href="style/LiveInspect.css"></stylesheet>)


       val mi = ManifestItem("1", "1", Seq(
         CssManifestResource("style/LiveInspect.css", "body{color: red}")
       ), <resource/>)
       val json = QtiTransformer.transform(qtiData, mi)

       val xml = XML.loadString( s"<root> ${(json \ "xhtml").as[String]}</root>" )
       (xml \\ "style")(1).text.trim must_== """/* style/LiveInspect.css */ .qti.kds body { color:red; }"""
     }
   }

 }

}
