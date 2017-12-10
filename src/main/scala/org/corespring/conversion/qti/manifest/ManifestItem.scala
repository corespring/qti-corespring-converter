package org.corespring.conversion.qti.manifest

import java.util.zip.ZipFile

import org.slf4j.LoggerFactory

import scala.xml.Node


case class ManifestItem(
                         id: String,
                         filename: String,
                         resources: Seq[ManifestResource] = Seq.empty, manifest: Node) {

  def qti : Option[QtiManifestResource] = resources.find(_.is(ManifestResourceType.QTI)).map( _.asInstanceOf[QtiManifestResource])

  def passages : Seq[PassageManifestResource] = resources.filter(_.is(ManifestResourceType.Passage)).map(_.asInstanceOf[PassageManifestResource])
}

/**
  * TODO: We are locating stylesheets in 2 possible places:
  *
  * 1. in the qti (this was added by ed to make sure measured progress picked up the stylesheet)
  * aka: <stylesheet href="blah"></stylesheet>
  * 2. in the resource node:
  * aka: <resource><file href=""></file></resource>
  *
  * Some issues with this:
  * - The path to the css can use relative paths from the qti eg: '../css/style.css' from item/qti.xml.
  *   - To get around this we use basename matching but it's pretty crude
  * - We don't support the measuredprogress <dependency> node type (is this part of Qti?)
  *
  * So we should try and load either way and even with different paths -
  * it should resolve to the same resource.
  *
  * We'll probably need to flesh out the model so we know the file's path in the archive when transforming.
  * We'll want access to the global manifest when building the data to support the <dependency> node.
  */

object ManifestItem {

  lazy val logger = LoggerFactory.getLogger(ManifestItem.this.getClass)

  def apply(node: Node, zip: ZipFile): ManifestItem = {
    ManifestItem(node, zip, _ => None)
  }

  def apply(resource: Node, zip: ZipFile, getId: Node => Option[String]): ManifestItem = {
    val qtiFile = (resource \ "@href").text.toString
    ZipReader.xml(zip, qtiFile).map{ qti =>
      val resources = ManifestResources.list(zip, resource, qtiFile, qti)
      val id = getId(resource).getOrElse((resource \ "@identifier").text.toString)
      ManifestItem(id, filename = qtiFile, resources = resources, resource)
    }.getOrElse(throw new RuntimeException("Can't find qti"))
  }
}
