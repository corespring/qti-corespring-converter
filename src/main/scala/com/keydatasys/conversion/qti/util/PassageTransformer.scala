package com.keydatasys.conversion.qti.util

import org.corespring.common.file.SourceWrapper
import org.corespring.common.util.HtmlProcessor
import org.corespring.conversion.qti.manifest.{ManifestResource, ManifestResourceType}
import org.slf4j.LoggerFactory

import scala.xml.XML

trait PassageTransformer extends PassageScrubber with HtmlProcessor with PathFlattener {

  val ptLogger = LoggerFactory.getLogger(PathTransformer.getClass)

  def transformPassage(resource: ManifestResource)(implicit sources: Map[String, SourceWrapper]): Option[String] = {
    resource.resourceType == ManifestResourceType.Passage match {
      case true => {
        sources.find{ case (path, source) => resource.path.flattenPath == path.flattenPath }.map(_._2) match {
          case Some(source) => try {
            Some(transformPassage(source.getLines.mkString))
          } catch {
            case t : Throwable => {
              ptLogger.error(s"Error reading passage ${resource.path}: ${t.getMessage}")
              None
            }
          }
          case _ => {
            println(s"Missing passage ${resource.path}")
            None
          }
        }
      }
      case _ => None
    }
  }

  private def transformPassage(xmlString: String): String =
    <div class="passage">{
      (XML.loadString(scrub(escapeEntities(xmlString))) \ "passageBody" \\ "passageParts" \\ "partBlock").map(pb => <div/>.copy(child = pb))
      }</div>.toString

}

