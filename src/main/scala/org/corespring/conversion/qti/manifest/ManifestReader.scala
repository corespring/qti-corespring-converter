package org.corespring.conversion.qti.manifest

import java.util.zip.ZipFile

import com.keydatasys.conversion.qti.util._
import org.corespring.common.file.SourceWrapper
import org.corespring.common.util.EntityEscaper
import org.corespring.conversion.qti.manifest._
import org.slf4j.LoggerFactory

import scala.xml._

object ManifestReader
  extends PassageScrubber
    with EntityEscaper
    with PathFlattener {

  val logger = LoggerFactory.getLogger(ManifestReader.this.getClass)

  val filename = "imsmanifest.xml"

  private def stripCDataTags(xmlString: String) =
    """(?s)<!\[CDATA\[(.*?)\]\]>""".r.replaceAllIn(xmlString, "$1")

  private def flattenPath(path: String) = {
    var regexes = Seq(
      ("""(\.\.\/)+(.*)""".r, "$2"),
      ("""\.\/(.*)""".r, "$1")
    )
    regexes.foldLeft(path)((acc, r) => r._1.replaceAllIn(acc, r._2))
  }

  def read(xml: Node, zip: ZipFile): QTIManifest = {
    val (qtiResources, resources) = (xml \ "resources" \\ "resource")
      .partition(r => (r \ "@type").text.toString == "imsqti_item_xmlv2p1")

    new QTIManifest(
      items = qtiResources.map(ManifestItem(_, zip)),
      otherFiles = resources.map(n => (n \ "@href").text.toString))
  }

  def read(xml: Node, sources: Map[String, SourceWrapper]): QTIManifest = {
    val (qtiResources, resources) = (xml \ "resources" \\ "resource")
      .partition(r => (r \ "@type").text.toString == "imsqti_item_xmlv2p1")

    val resourceLocators: Map[ManifestResourceType.Value, Node => Seq[String]] =
      Map(
        ManifestResourceType.Image -> (n => (n \\ "img").map(_ \ "@src").map(_.toString)),
        ManifestResourceType.Video -> (n => (n \\ "video").map(_ \ "source").map(_ \ "@src").map(_.toString)),
        ManifestResourceType.Audio -> (n => (n \\ "audio").map(_ \ "source").map(_ \ "@src").map(_.toString)),
        ManifestResourceType.StyleSheet -> (n => (n \\ "stylesheet").map(_ \ "@href").map(_.toString)))

    new QTIManifest(items =
      qtiResources.map(n => {
        val filename = (n \ "@href").text.toString
        logger.debug(s">>> filename: $filename")
        val files = sources.get(filename).map { file =>
          try {
            Some(XML.loadString(scrub(escapeEntities(stripCDataTags(file.mkString)))))
          } catch {
            case e: Exception => {
              e.printStackTrace()
              println(s"Error reading: $filename")
              None
            }
          }
        }.flatten.map(node => {
          resourceLocators.map { case (resourceType, fn) => resourceType -> fn(node) }
        }).getOrElse(Map.empty[ManifestResourceType.Value, Seq[String]]).map {
          case (resourceType, filenames) => {

            logger.debug(s">>> filenames: $filenames")
            filenames.map(filename => {
              if (filename.contains("134580A-134580A_cubes-box_stem_01.png")) {
                println(flattenPath(filename))
              }
              ManifestResource(
                path = flattenPath(filename),
                resourceType = resourceType,
                isInline = false)
            })
          }
        }.flatten.toSeq
        logger.debug(s">>> files: ${files.map(_.path)}")
        val resources = ((n \\ "file")
          .filterNot(f => (f \ "@href").text.toString == filename).map(f => {
          val path = (f \ "@href").text.toString
          ManifestResource(
            path = path,
            resourceType = ManifestResourceType.fromPathOld(path)(xml),
            isInline = false)
        })) ++ files :+ ManifestResource(
          filename,
          ManifestResourceType.QTI,
          isInline = false)

        val passageResources: Seq[ManifestResource] = resources.filter(resource => resource.is(ManifestResourceType.Passage) || resource.is(ManifestResourceType.QTI)).map(p =>
          sources.find { case (path, _) => path == p.path.flattenPath }.map {
            case (filename, s) => {
              try {
                Some((XML.loadString(scrub(escapeEntities(stripCDataTags(s.getLines.mkString))))).map(xml => resourceLocators.map {
                  case (resourceType, fn) => (resourceType, fn(xml))
                }).flatten.map {
                  case (resourceType, paths) =>
                    paths.map(path => ManifestResource(
                      path = flattenPath(path),
                      resourceType = resourceType,
                      isInline = false))
                }.flatten)
              } catch {
                case e: Exception => {
                  println(s"Error reading: $filename")
                  e.printStackTrace
                  None
                }
              }
            }
          }.flatten).flatten.flatten

        val missingPassageResources = passageResources.map(_.path).filter(path => sources.get(path.flattenPath).isEmpty)
        if (missingPassageResources.nonEmpty) {
          missingPassageResources.foreach(f => logger.warn(s"Missing file $f in uploaded import"))
        }

        ManifestItem(id = (n \ "@identifier").text.toString, filename = filename, resources = resources ++ passageResources, n)
      }),
      otherFiles = resources.map(n => (n \ "@href").text.toString))
  }

}
