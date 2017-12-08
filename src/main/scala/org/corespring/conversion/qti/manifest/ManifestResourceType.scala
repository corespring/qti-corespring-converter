package org.corespring.conversion.qti.manifest

import org.slf4j.LoggerFactory

import scala.xml.Node

object ManifestResourceType extends Enumeration {

  lazy val logger = LoggerFactory.getLogger(ManifestResourceType.this.getClass)

  type ManifestResourceType = Value
  val QTI, Passage, Video, Audio, Image, StyleSheet, Unknown = Value

  private val typeMap = Map(
    "imsqti_item_xmlv2p1" -> QTI,
    "passage" -> Passage)

  private val extensionMap = Map(
    Seq("gif", "jpeg", "jpg", "png") -> Image,
    Seq("mp3") -> Audio,
    Seq("css") -> StyleSheet)

  private def fromPathString(path: String): ManifestResourceType.Value =  if( path.startsWith("passages/")){
    ManifestResourceType.Passage
  } else {
    val extension = path.split("\\.").lastOption.getOrElse("").toLowerCase
    logger.trace(s"path: $path, extension: $extension")
    extensionMap.find{ case (extensions, t) => extensions.contains(extension)}
      .map(_._2).getOrElse(Unknown)
  }


  def fromPath(path:String, resource: Node): ManifestResourceType.Value = {
    val rawType = (resource \ "@type").headOption.map(_.text)
    logger.trace(s"rawType: $rawType")
    rawType.flatMap(typeMap.get(_)).getOrElse(fromPathString(path))
  }

}