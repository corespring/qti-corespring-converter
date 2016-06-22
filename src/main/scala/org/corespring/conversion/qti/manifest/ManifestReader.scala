package org.corespring.conversion.qti.manifest

import com.keydatasys.conversion.qti.util._
import org.corespring.common.file.SourceWrapper
import org.corespring.conversion.qti.manifest._

import scala.util.logging.Logged
import scala.xml._

object ManifestReader
    extends PassageScrubber
    with EntityEscaper
    with PathFlattener
    with Logged
{

  val filename = "imsmanifest.xml"

  private def stripCDataTags(xmlString: String) =
    """(?s)<!\[CDATA\[(.*?)\]\]>""".r.replaceAllIn(xmlString, "$1")

  def read(xml: Node, sources: Map[String, SourceWrapper]): QTIManifest = {
    val (qtiResources, resources) = (xml \ "resources" \\ "resource")
      .partition(r => (r \ "@type").text.toString == "imsqti_item_xmlv2p1")

    val resourceLocators: Map[ManifestResourceType.Value, Node => Seq[String]] =
      Map(
        ManifestResourceType.Image -> (n => (n \\ "img").map(_ \ "@src").map(_.toString)),
        ManifestResourceType.Video -> (n => (n \\ "video").map(_ \ "source").map(_ \ "@src").map(_.toString)),
        ManifestResourceType.Audio -> (n => (n \\ "audio").map(_ \ "source").map(_ \ "@src").map(_.toString)))

    new QTIManifest(items =
      qtiResources.map(n => {
        val filename = (n \ "@href").text.toString
        val files = sources.get(filename).map { file =>
          try {
            Some(XML.loadString(scrub(escapeEntities(stripCDataTags(file.mkString)))))
          } catch {
            case e: Exception => {
              println(s"Error reading: $filename")
              None
            }
          }
        }.flatten.map(node => {
          resourceLocators.map { case (resourceType, fn) => resourceType -> fn(node) }.toMap
        }).getOrElse(Map.empty[ManifestResourceType.Value, Seq[String]]).map {
          case (resourceType, filenames) => {
            filenames.map(filename => ManifestResource(path = """\.\/(.*)""".r.replaceAllIn(filename, "$1"), resourceType = resourceType))
          }
        }.flatten.toSeq

        val resources = ((n \\ "file")
          .filterNot(f => (f \ "@href").text.toString == filename).map(f => {
          val path = (f \ "@href").text.toString
          ManifestResource(
            path = path,
            resourceType = ManifestResourceType.fromPath(path)(xml))
        })) ++ files :+ ManifestResource(filename, ManifestResourceType.QTI)

        val passageResources: Seq[ManifestResource] = resources.filter(resource => resource.is(ManifestResourceType.Passage) || resource.is(ManifestResourceType.QTI)).map(p =>
          sources.find { case (path, _) => path == p.path.flattenPath }.map {
            case (filename, s) => {
              try {
                Some((XML.loadString(scrub(escapeEntities(stripCDataTags(s.getLines.mkString))))).map(xml => resourceLocators.map {
                  case (resourceType, fn) => (resourceType, fn(xml))
                }).flatten.map {
                  case (resourceType, paths) =>
                    paths.map(path => ManifestResource(path = """\.\/(.*)""".r.replaceAllIn(path, "$1"), resourceType = resourceType))
                }.flatten)
              } catch {
                case e: Exception => {
                  println(s"Error reading: $filename")
                  println(scrub(escapeEntities(stripCDataTags(s.getLines.mkString))))
                  e.printStackTrace
                  None
                }
              }
            }
          }.flatten).flatten.flatten

        val missingPassageResources = passageResources.map(_.path).filter(path => sources.get(path.flattenPath).isEmpty)
        if (missingPassageResources.nonEmpty) {
          missingPassageResources.foreach(f => log(s"Missing file $f in uploaded import"))
        }

        ManifestItem(id = (n \ "@identifier").text.toString, filename = filename, resources = resources ++ passageResources, n)
      }),
      otherFiles = resources.map(n => (n \ "@href").text.toString))
  }

}
