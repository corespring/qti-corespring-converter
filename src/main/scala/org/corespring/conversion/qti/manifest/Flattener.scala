package org.corespring.conversion.qti.manifest

import org.corespring.common.xml.XhtmlParser
import org.corespring.utils.CDataHelper
import org.slf4j.LoggerFactory

import scala.xml.Node

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
      XhtmlParser.loadString(fixed)
    } catch {
      case t : Throwable => {
        logger.error(fixed)
        t.printStackTrace()
        throw new RuntimeException(s"Error flattening xml...${t.getMessage}")
      }
    }
  }
}
