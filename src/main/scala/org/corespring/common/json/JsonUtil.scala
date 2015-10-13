package org.corespring.common.json

import play.api.libs.json._

import scala.reflect.ClassTag
import scala.xml.Node


trait JsonUtil {

  /**
   * Returns an Option of JsValue subtype T for an attribute of the implicit node. For example:
   *
   *   implicit val node = <span class="great" count=2 awesome=true>Test</span>
   *
   *   optForAttr[JsString]("class")    // Some(JsString(great))
   *   optForAttr[JsNumber]("count")    // Some(JsNumber(2))
   *   optForAttr[JsBoolean]("awesome") // Some(JsBoolean(true))
   *   optForAttr[JsString]("id")       // None
   */
  def optForAttr[T <: JsValue](attr: String)(implicit node: Node, mf: ClassTag[T]) = {
    (node \ s"@$attr") match {
      case empty: Seq[Node] if empty.isEmpty => None
      case nonEmpty: Seq[Node] if (classOf[JsNumber] isAssignableFrom mf.runtimeClass) =>
        Some(JsNumber(BigDecimal(nonEmpty.head.text)))
      case nonEmpty: Seq[Node] if (classOf[JsBoolean] isAssignableFrom mf.runtimeClass) =>
        Some(JsBoolean(nonEmpty.head.text.toBoolean))
      case nonEmpty: Seq[Node] => Some(JsString(nonEmpty.head.text.toString))
    }
  }

  /**
   * Returns a JsObject with only the fields whose values are Some(JsValue)
   */
  def partialObj(fields: (String, Option[JsValue])*): JsObject =
    JsObject(fields.filter { case (_, v) => v.nonEmpty }.map { case (a, b) => (a, b.get) })

}
