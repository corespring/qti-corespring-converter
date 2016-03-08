package org.corespring.conversion.qti.manifest

import scala.xml.Node

object ManifestResourceType extends Enumeration {
  type ManifestResourceType = Value
  val QTI, Passage, Video, Image, Unknown = Value

  private val typeMap = Map(
    "imsqti_item_xmlv2p1" -> QTI,
    "imsqti_apipitem_xmlv2p1" -> QTI,
    "passage" -> Passage)

  private val extensionMap = Map(Seq("gif", "jpeg", "jpg", "png", "svgz") -> Image)
  private val pathFunctions: Seq[String => Option[ManifestResourceType.Value]] = Seq(
    (path => path.startsWith("passages/") match {
      case true => Some(ManifestResourceType.Passage)
      case _ => None
    }))

  private def fromPathString(path: String): ManifestResourceType.Value = {
    def getExtension(path: String) = path.split("\\.").lastOption.getOrElse("").toLowerCase
    pathFunctions.map(_(path)).find(_.nonEmpty).flatten.getOrElse(
      extensionMap.find { case (extensions, resourceType) => extensions.contains(getExtension(path)) }
        .map(_._2).getOrElse(Unknown))
  }

  def fromPath(path: String)(implicit xml: Node): ManifestResourceType.Value =
    (xml \ "resources" \\ "resource").find(resource => (resource \ "@href").text.toString == path)
      .map(resource => (resource \ "@type").text.toString).map(typeMap.get(_)).flatten.getOrElse(fromPathString(path))

}