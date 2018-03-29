package com.keydatasys.conversion.qti

import com.keydatasys.conversion.qti.util.{PassageTransformer, PathTransformer, TableTransformer}
import org.corespring.common.file.SourceWrapper
import org.corespring.common.xml.XMLNamespaceClearer
import org.corespring.conversion.qti.{QtiTransformer => SuperQtiTransformer}
import org.corespring.conversion.qti.manifest._
import org.slf4j.LoggerFactory
import play.api.libs.json._
import org.corespring.macros.DescribeMacro._


import scala.xml._
import scala.xml.transform._

//TODO: should be private[keydatasys]
class KDSItemTransformer(qtiTransformer: SuperQtiTransformer) extends PassageTransformer {

  private val logger = LoggerFactory.getLogger(this.getClass)
  def transform(xmlString: String, manifestItem: ManifestItem, sources: Map[String, SourceWrapper]): JsValue = {
    val passages: Seq[String] = manifestItem.resources.filter(_.resourceType == ManifestResourceType.Passage)
      .map(transformPassage(_)(sources).getOrElse(""))
    val passageXml = passages.length match {
      case 1 => passages.head
      case _ => s"<div>${passages.mkString}</div>"
    }
    try {
      val xml = TableTransformer.transform(PathTransformer.transform(xmlString.toXML(passageXml)))
      logger.info(describe(sources))
      val out = qtiTransformer.transform(xml, sources, manifestItem.manifest)
      logger.trace(describe(out))
      out
    } catch {
      case e: Exception => {
        throw e
      }
    }
  }

  /**
   * Maps some KDS QTI nodes to valid HTML nodes, and cleans up namespaces.
   */
  implicit class XMLCleaner(string: String) extends XMLNamespaceClearer {

    private val labelMap = Map("partBlock" -> "div", "partBody" -> "div", "selectedResponseParts" -> "div")

    def toXML(passageXml: String): Elem = {
      def stripCDataTags(xmlString: String) = """(?s)<!\[CDATA\[(.*?)\]\]>""".r.replaceAllIn(xmlString, "$1")
      val xml = XML.loadString(stripCDataTags(string))
      val stylesheets = (xml \ "stylesheet")
      clearNamespace(new RuleTransformer(new RewriteRule {
        override def transform(n: Node): NodeSeq = n match {
          case n: Elem if (n.label == "itemBody") => passageXml.nonEmpty match {
            case true => n.copy(child = stylesheets ++ XML.loadString(passageXml) ++ n.child)
            case _ => n.copy(child = stylesheets ++ n.child)
          }
          case _ => n
        }
      }).transform(xml).headOption.getOrElse(throw new Exception("There was no head element!"))).head.asInstanceOf[Elem]
    }

    def removeResponseProcessing(node: Node): Node = {
      new RuleTransformer(new RewriteRule {
        override def transform(n: Node) = n match {
          case n: Node if (n.label == "responseProcessing") => Seq.empty
          case _ => n
        }
      }).transform(node).head
    }

  }


}

//TODO: should be private[keydatasys]
//object KDSItemTransformer extends KDSItemTransformer(KDSQtiTransformer)