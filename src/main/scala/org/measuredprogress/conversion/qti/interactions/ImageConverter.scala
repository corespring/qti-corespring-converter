package org.measuredprogress.conversion.qti.interactions

import org.corespring.common.html.JsoupParser
import org.jsoup.Jsoup
import org.jsoup.nodes.{Entities, Document}
import play.api.libs.json.{JsString, JsArray, JsObject, JsValue}

import scala.collection.JavaConversions._
import scala.xml._
import scala.xml.transform._

/**
  * Sometimes Measured Progress like to put images in <object data="image.png" type="image/png"/> instead of
  * <img src="image.png"/>, so we have to convert these.
  */
trait ImageConverter {

  def convertObjectsToImages(html: NodeSeq): NodeSeq = {
    new RuleTransformer(new RewriteRule() {
      override def transform(node: Node): Seq[Node] = node match {
        case e: Elem if (e.label == "object") => (e.attribute("type"), e.attribute("data")) match {
          case (Some(objType), Some(data)) if (objType.text == "image/png") => <img src={data.toString.split("/").last}/>
          case _ => node
        }
        case _ => node
      }
    }).transform(html)
  }

  def convertHtml(html: String): String = {
    /** Note: we parse as xml so as not to corrupt the markup structure */
    val doc = JsoupParser.parseXml(html)
    doc.select("object").foreach(obj => {
      obj.attr("type") match {
        case "image/png" => {
          val img = doc.createElement("img")
          img.attr("src", obj.attr("data").split("/").last)
          obj.replaceWith(img)
        }
        case _ => {}
      }
    })
    doc.outerHtml()
  }

}
