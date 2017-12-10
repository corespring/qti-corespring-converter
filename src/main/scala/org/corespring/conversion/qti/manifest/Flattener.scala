package org.corespring.conversion.qti.manifest

import org.corespring.utils.CDataHelper
import org.slf4j.LoggerFactory

import scala.io.Source
import scala.xml.parsing.ConstructingParser
import scala.xml.{Node, XML}

object Flattener {

  private val logger = LoggerFactory.getLogger(Flattener.this.getClass)
  /**
    * Remove CDATA and expose contents as part of the markup tree.
    * Escape the contents and fix markup so the content is parseable
    * @param node
    * @return
    */
  def flatten(node:Node): Node = {
    val raw = node.toString
    val stripped = CDataHelper.stripCDataTags(raw)
    val fixed = CDataHelper.fixXml(stripped)

    logger.debug(s"fixed: $fixed")
    try {
      val p = ConstructingParser.fromSource(Source.fromString(fixed), false)
      p.document.docElem
    } catch {
      case t : Throwable => {
        logger.error(fixed)
        t.printStackTrace()
        throw new RuntimeException(s"Error flattening xml...${t.getMessage}")
      }
    }
  }
}
