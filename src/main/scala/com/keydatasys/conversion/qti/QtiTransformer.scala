package com.keydatasys.conversion.qti

import org.corespring.common.file.SourceWrapper
import org.corespring.conversion.qti.AbstractQtiTransformer
import play.api.libs.json.JsValue

import scala.xml.{Node, Elem}

object QtiTransformer extends AbstractQtiTransformer {

  override def transform(qti: Elem): JsValue = ???

  override def transform(qti: Elem, sources: Map[String, SourceWrapper], manifest: Node): JsValue = ???

}
