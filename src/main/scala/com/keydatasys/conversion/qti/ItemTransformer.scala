package com.keydatasys.conversion.qti

import com.keydatasys.conversion.qti.util.{PathTransformer, TableTransformer}
import org.corespring.conversion.qti.{QtiTransformer => SuperQtiTransformer}
import org.corespring.conversion.qti.manifest._
import org.slf4j.LoggerFactory
import play.api.libs.json._
import org.corespring.macros.DescribeMacro._


import scala.xml._
import scala.xml.transform._

private class AddPassageToXml(passages:Seq[Node]) extends RewriteRule{

  def blockToDiv(p:Node) : Seq[Node] = (p \ "passageBody" \\ "passageParts" \\ "partBlock").map(pb => <div/>.copy(child = pb))

  lazy val blockContent : Option[Node] = {
    val nodes = passages.map(p => blockToDiv(p)).flatten
    if(nodes.length > 0) {
      Some(<div class="passage">{nodes}</div>)
    } else {
      None
    }
  }

  override def transform(n:Node) = {
    n match {
      case e: Elem if e.label == "itemBody" => e.copy(child = blockContent.getOrElse(Seq.empty) ++ n.child )
      case _ => n
    }
  }
}

//TODO: should be private[keydatasys]
class ItemTransformer(qtiTransformer: SuperQtiTransformer) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def transform(qti: Node, manifestItem: ManifestItem): JsValue = {
    try {
      val transformer = new RuleTransformer(
        new AddPassageToXml(manifestItem.passages.map(_.xml)),
        PathTransformer.rule,
        TableTransformer.rule
      )
      val result = transformer.transform(qti)
      val out = qtiTransformer.transform(result.head.asInstanceOf[Elem], manifestItem)
      logger.trace(describe(out))
      out
    } catch {
      case t: Throwable => {
        logger.error(s"Error with transform: ${t.getMessage}")
        throw new RuntimeException(s"Error running transform ${manifestItem.id}: ${t.getMessage}", t)
      }
    }
  }
}

//TODO: should be private[keydatasys]
object ItemTransformer extends ItemTransformer(QtiTransformer)