package org.corespring.conversion.qti

import org.corespring.common.file.SourceWrapper
import org.corespring.common.xml.XMLNamespaceClearer
import play.api.libs.json.JsValue

import scala.xml.{Node, Elem}

abstract class AbstractQtiTransformer extends XMLNamespaceClearer {

  def transform(qti: Elem): JsValue

  def transform(qti: Elem, sources: Map[String, SourceWrapper] = Map.empty[String, SourceWrapper], manifest: Node = <div/>): JsValue


}