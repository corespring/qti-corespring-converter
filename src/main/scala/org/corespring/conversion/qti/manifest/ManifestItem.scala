package org.corespring.conversion.qti.manifest

import java.util.zip.ZipFile

import com.keydatasys.conversion.qti.util.PassageScrubber
import org.corespring.common.util.EntityEscaper
import org.corespring.macros.DescribeMacro.describe
import org.slf4j.LoggerFactory

import scala.xml.Node


case class ManifestItem(
                         id: String,
                         filename: String,
                         resources: Seq[ManifestResource] = Seq.empty, manifest: Node) {

  def qti : Option[QtiManifestResource] = resources.find(_.is(ManifestResourceType.QTI)).map( _.asInstanceOf[QtiManifestResource])
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

object ManifestItem extends PassageScrubber with EntityEscaper {

  lazy val logger = LoggerFactory.getLogger(ManifestItem.this.getClass)

  val resourceLocators: Map[ManifestResourceType.Value, Node => Seq[String]] =
    Map(
      ManifestResourceType.Image -> (n => (n \\ "img").map(_ \ "@src").map(_.toString)),
      ManifestResourceType.Video -> (n => (n \\ "video").map(_ \ "source").map(_ \ "@src").map(_.toString)),
      ManifestResourceType.Audio -> (n => (n \\ "audio").map(_ \ "source").map(_ \ "@src").map(_.toString)),
      ManifestResourceType.StyleSheet -> (n => (n \\ "stylesheet").map(_ \ "@href").map(_.toString))

    )

  private def flattenPath(path: String) = {
    var regexes = Seq(
      ("""(\.\.\/)+(.*)""".r, "$2"),
      ("""\.\/(.*)""".r, "$1")
    )
    regexes.foldLeft(path)((acc, r) => r._1.replaceAllIn(acc, r._2))
  }


  def apply(node: Node, zip: ZipFile): ManifestItem = {
    ManifestItem(node, zip, _ => None)
  }

  def apply(resource: Node, zip: ZipFile, getId: Node => Option[String]): ManifestItem = {

    val qtiFile = (resource \ "@href").text.toString
    val files = ZipReader.qti(zip, qtiFile).map(qti => {
      logger.trace(s"node: ${resource.toString}")
      logger.trace(s"qti: ${qti.toString}")
      resourceLocators.map { case (resourceType, fn) => resourceType -> fn(qti) }
    }).getOrElse(Map.empty[ManifestResourceType.Value, Seq[String]]).flatMap {
      case (resourceType, filenames) => {
        filenames.map(filename => {
          if (filename.contains("134580A-134580A_cubes-box_stem_01.png")) {
            logger.error(s"?? ${flattenPath(filename)}")
          }
          val out = ManifestResource(
            path = flattenPath(filename),
            resourceType = resourceType,
            //We inline css
            inline = resourceType == ManifestResourceType.StyleSheet
          )

          logger.debug(describe(out))
          out
        })
      }
    }.toSeq

    logger.debug(s"files: ${files}")

    val resources = ((resource \\ "file")
      .filterNot(f => (f \ "@href").text.toString == qtiFile).map(f => {
      val path = (f \ "@href").text.toString
      val resourceType = ManifestResourceType.fromPath(path, f)
      ManifestResource(
        path,
        resourceType,
        inline = resourceType == ManifestResourceType.StyleSheet)

    })) ++ files :+ ManifestResource(
      qtiFile,
      ManifestResourceType.QTI,
      inline = false)

    logger.debug(s"resources: $resources")

    val unLoadedPassageResources: Seq[ManifestResource] = resources.filter(resource => resource.is(ManifestResourceType.Passage))

    def toXml(mr: ManifestResource): Option[Node] = ZipReader.fileXML(zip, mr.path)

    /**
      * if there's a resource in the passage markup add it to the main Manifest Item?
      */
    val passageResources = unLoadedPassageResources
      .flatMap(toXml)
      .flatMap(xml => resourceLocators.map {
        case (resourceType, fn) => (resourceType, fn(xml))
      })
      .flatMap {
        case (resourceType, paths) =>
          paths.map { path =>
            ManifestResource(
              path = flattenPath(path),
              resourceType = resourceType,
              inline = resourceType == ManifestResourceType.StyleSheet)
          }
      }


    logger.debug(describe(passageResources))

    val missingPassageResources = passageResources.map(_.path).filter(path => zip.getEntry(path) == null)

    if (missingPassageResources.nonEmpty) {
      missingPassageResources.foreach(f => logger.warn(s"Missing file $f in uploaded import"))
    }

    val out = (resources ++ passageResources).distinct
    logger.debug(s"out: $out")
    val id = getId(resource).getOrElse((resource \ "@identifier").text.toString)

    ManifestItem(id, filename = qtiFile, resources = out, resource)
  }
}
