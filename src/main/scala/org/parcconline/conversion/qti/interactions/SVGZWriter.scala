package org.parcconline.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json._

import scala.xml._

object SVGZWriter extends InteractionTransformer {

  val attributeSvgzRegex = "(.*)\\.svgz$".r

  override def interactionJs(qti: Node, manifest: Node): Map[String, JsObject] = Map.empty

  override def transform(node: Node, manifest: Node): Seq[Node] = node.isSVGZ match {
    case true => node.rewriteSrcAsPng
    case _ => node
  }

  private def rewriteUrls(string: String): String = {
    string.replaceAllLiterally(".svgz", ".png")
  }

  def rewriteUrls(jsValue: JsValue): JsValue = jsValue match {
    case jsObject: JsObject => JsObject(jsObject.fields.map{ case (key, value) => {
      (key, rewriteUrls(value))
    } })
    case jsArray: JsArray => JsArray(jsArray.value.map{ value => rewriteUrls(value) })
    case jsString: JsString => JsString(rewriteUrls(jsString.value))
    case _ => jsValue
  }

  implicit class SVGZNode(node: Node) {

    private def svgzFilenamePrefix = node.attribute("src") match {
      case Some(Text(url)) => url match {
        case attributeSvgzRegex(prefix) => Some(prefix)
        case _ => None
      }
      case _ => None
    }

    implicit class AttributeElem(elem: Elem) {
      def setAttribute(key: String, value: String) = elem % new UnprefixedAttribute(key, value, Null)
    }

    def isSVGZ = svgzFilenamePrefix.nonEmpty

    def rewriteSrcAsPng = (node, svgzFilenamePrefix) match {
      case (node: Elem, Some(prefix)) => node.setAttribute("src", s"$prefix.png")
      case _ => node
    }

  }

}
