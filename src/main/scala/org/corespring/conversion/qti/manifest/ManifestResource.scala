package org.corespring.conversion.qti.manifest

import java.nio.file.Paths

import org.corespring.conversion.qti.manifest.ManifestResourceType.ManifestResourceType

import scala.xml.Node


trait ManifestResource {
  def path: String

  def resourceType: ManifestResourceType.Value

  def inline: Boolean

  def is(resourceType: ManifestResourceType.Value) = this.resourceType == resourceType
}

case class CssManifestResource(
                                path: String, src: String
                              ) extends ManifestResource {
  val resourceType = ManifestResourceType.StyleSheet
  val inline = true
}

case class SimpleManifestResource(
                                   path: String,
                                   resourceType: ManifestResourceType.Value,
                                   inline: Boolean
                                 ) extends ManifestResource {
}

case class QtiManifestResource(path: String, qti: Node) extends ManifestResource {
  val resourceType = ManifestResourceType.QTI

  val inline = false
}

case class PassageManifestResource(path: String, xml: Node) extends ManifestResource {
  override def resourceType = ManifestResourceType.Passage

  //TODO: this is not strictly true??
  override def inline = false
}

object ManifestResource {


  def normalizePath(s:String) = Paths.get(s).normalize().toString

  def apply(path: String, resourceType: ManifestResourceType, inline: Boolean): ManifestResource = {
    SimpleManifestResource(
      Paths.get(path).normalize().toString, resourceType, inline)
  }

  private def pull(label: String, getValue: Node => Seq[String])(node: Node): Seq[String] = {
    val targets = (node \\ label)
    val values: Seq[Seq[String]] = targets.map(getValue)
    values.flatten.filterNot(s => s == null || s.isEmpty)
  }

  private def getSrces(n: Node): Seq[String] = (n \\ "source").map(s => (s \ "@src").text).toList

  private val typeToSrcExtractor = Map(
    ManifestResourceType.Image -> pull("img", (n => Seq((n \ "@src").text))) _,
    ManifestResourceType.Video -> pull("video", getSrces) _,
    ManifestResourceType.Audio -> pull("audio", getSrces) _,
    ManifestResourceType.StyleSheet -> pull("stylesheet", (s => Seq((s \ "@href").text))) _
  )

  /**
    * Extract a map of sources per manifest type
    *
    * @param node the qti/passage xml
    * @return a map of sources per type
    */
  @deprecated("whats this for?", "")
  def extractSources(node: Node): Map[ManifestResourceType, Seq[String]] = {
    typeToSrcExtractor.map {
      case (manifestType, extractFn) => manifestType -> extractFn(node)
    }
  }
}