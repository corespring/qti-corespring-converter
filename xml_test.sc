import java.io.File

import scala.io.Source
import scala.xml.XML
import scala.xml.parsing.ConstructingParser
import org.corespring.conversion.qti.manifest.Flattener


//val test = <root><![CDATA[a < b &radic;]]><a>--hi !!--!!</a><![CDATA[this is the end]]></root>
//test.text
//test.toString()
val testTwoString = "<root><![CDATA[a < b &radic;]]><a>--hi !!--!!</a><![CDATA[this is the end]]></root>"
val testTwo = ConstructingParser.fromSource(Source.fromString(testTwoString), false)
val flattened = Flattener.flatten(testTwo.document.docElem)
val ttString = testTwo.document().docElem.toString()
//testTwo.document().docElem.text
//val xmlFile = new File("/Users/edeustace/dev/github/corespring/qti-corespring-converter/src/it/resources/665204/tmp.xml")
//val parser = ConstructingParser.fromFile(xmlFile, false)
//val root = parser.document()
//
//
//val p = new scala.xml.PrettyPrinter(80, 4)
//val string = p.format(root.docElem)
//
//val rp = (root \\ "responseDeclaration")
//
//
//(root \ "itemBody").head.child.text
//
//rp.foreach( r => {
//   val cr =  (r \\ "correctResponse" \ "value")
//   println(s"cr: ${cr}")
//   cr.foreach( c => println(c.toString, c.text))
//})