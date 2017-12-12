import java.io.File

import scala.io.Source
import scala.xml.{Elem, Node, XML}
import scala.xml.parsing.ConstructingParser
import org.corespring.conversion.qti.manifest.Flattener
import org.corespring.utils.CDataHelper
import org.jsoup.Jsoup
import org.jsoup.nodes.Document.OutputSettings
import org.jsoup.nodes.Entities.EscapeMode
import org.jsoup.parser.Parser

import scala.xml.transform.RewriteRule

def toXml(s:String) : Node = {
  ConstructingParser.fromSource(scala.io.Source.fromString(s), false).document.docElem
}

val x = """<prompt visible="true"><![CDATA[blah<audio controls>
          |  <source src="./audio/677403s.mp3" type="audio/mpeg">
          |  <span class="kds-noprint">Your browser does not support the audio tag.</span>
          |</audio>]]></prompt>""".stripMargin

val node = toXml(x)

val html = """<p class=\"prompt\"> \n    <corespring-audio id=\"677403s\" /> </p>"""
val r = """<(.*)/>""".r
r.findAllIn(html).length
val htmlFixed = r.replaceAllIn(html, m => {
  println(s"m: $m")
  val one = m.group(1)
  println(s"one>>$one")
  val Seq(name, rest) = if(one.contains(" ")) one.split(" ") else Seq(one, "")
  s"<$name $rest></$name>"
})
val rule = new RewriteRule {
  override def transform(n:Node)= {
    n match {
      case e : Elem if e.text.indexOf("<audio") != -1 => {
        val stripped = CDataHelper.stripCDataTags(e.text)
        val empty = e.copy(child = Seq(<contents_here/>))
        val xmlMarkup = CDataHelper.fixXml(empty.toString.replaceFirst("<contents_here/>", stripped))
        val p = ConstructingParser.fromSource(Source.fromString(xmlMarkup), false)
        p.document().docElem
      }
      case _ => n
    }
  }
}

val out = rule.transform(node)
out.toString()
val y = "foo"
//val x = """<assessmentItem>
//  <responseDeclaration identifier="Q_01" cardinality="single" baseType="string">
//    <correctResponse>
//      <value>hi</value>
//    </correctResponse>
//  </responseDeclaration>
//  <itemBody>
//    <p>This is some info that's in the prompt</p>
//    <textEntryInteraction responseIdentifier="Q_01" expectedLength="15" popupFeedback="true" />
//    <feedbackBlock outcomeIdentifier="responses.Q_01.value" identifier="someCorrect"><div>correct</div></feedbackBlock>
//    <feedbackBlock outcomeIdentifier="responses.Q_01.value" incorrectResponse="true" ><div>incorrect</div></feedbackBlock>
//    <feedbackBlock outcomeIdentifier="responses.someOther.value" incorrectResponse="true" ><div>incorrect</div></feedbackBlock>
//  </itemBody>
//</assessmentItem>"""

          toXml(x)
//val html =
//  """
//    |<style>
//    |  .table > tr {
//    |    color:red;
//    |  }
//    |</style>
//    |<div>Hi</div>
//  """.stripMargin
//val htmlParser = Parser.htmlParser()
//val css = <style> .a > b {} </style>
//val doc = Jsoup.parse(html, "", Parser.htmlParser())
//doc.outputSettings(
//  new OutputSettings()
//    .syntax(OutputSettings.Syntax.html)
//      .escapeMode(EscapeMode.xhtml))
//doc.outerHtml()
//doc.head().html()
//doc.body().html()
//val pb =
//  """<partblock type="instruction"><![CDATA[
//    |<strong>Read the text and answer the following question(s).</strong>]]>
//    |</partblock>""".stripMargin
////val test = <root><![CDATA[a < b &radic;]]><a>--hi !!--!!</a><![CDATA[this is the end]]></root>
////test.text
////test.toString()
//val testTwoString = "<root><![CDATA[<bb/>]]><![CDATA[a < b &radic;]]><a>--hi !!--!!</a><![CDATA[this is the end]]></root>"
//val testTwo = ConstructingParser.fromSource(Source.fromString(pb), false)
//testTwo.document.docElem.toString
////(testTwo.document.docElem).text
////val s = testTwo.document.docElem.text
////val flattened = Flattener.flatten(testTwo.document.docElem)
////val ttString = testTwo.document().docElem.toString()
////testTwo.document().docElem.text
////val xmlFile = new File("/Users/edeustace/dev/github/corespring/qti-corespring-converter/src/it/resources/665204/tmp.xml")
////val parser = ConstructingParser.fromFile(xmlFile, false)
////val root = parser.document()
////
////
////val p = new scala.xml.PrettyPrinter(80, 4)
////val string = p.format(root.docElem)
////
////val rp = (root \\ "responseDeclaration")
////
////
////(root \ "itemBody").head.child.text
////
////rp.foreach( r => {
////   val cr =  (r \\ "correctResponse" \ "value")
////   println(s"cr: ${cr}")
////   cr.foreach( c => println(c.toString, c.text))
////})