package org.corespring.conversion.qti.manifest

import org.corespring.utils.CDataHelper

import scala.xml.{Node, XML}

object Flattener {

  /**
    * Remove CDATA and expose contents as part of the markup tree.
    * Escape the contents and fix markup so the content is parseable
    * @param node
    * @return
    */
  def flatten(node:Node) = {
    val raw = node.toString
    val fixed = CDataHelper.stripCDataAndEscapeIfNeeded(raw)
    XML.loadString(fixed)
  }
}
