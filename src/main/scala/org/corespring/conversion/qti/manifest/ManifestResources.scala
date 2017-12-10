package org.corespring.conversion.qti.manifest

import java.util.zip.ZipFile

import org.slf4j.LoggerFactory
import org.corespring.macros.DescribeMacro.describe
import org.corespring.utils.CDataHelper

import scala.xml.{Node, XML}



object ManifestResources{

  private val logger = LoggerFactory.getLogger(ManifestResources.this.getClass)

  private val resourceLocators: Map[ManifestResourceType.Value, Node => Seq[String]] =
    Map(
      ManifestResourceType.Image -> (n => (n \\ "img").map(_ \ "@src").map(_.toString)),
      ManifestResourceType.Video -> (n => (n \\ "video").map(_ \ "source").map(_ \ "@src").map(_.toString)),
      ManifestResourceType.Audio -> (n => (n \\ "audio").map(_ \ "source").map(_ \ "@src").map(_.toString)),
      ManifestResourceType.StyleSheet -> (n => (n \\ "stylesheet").map(_ \ "@href").map(_.toString))
    )

  /**
    * Build a list of manifest resources.
    * Look in the <resource> node, the qti and any passages.
    * @param zip
    * @param resourceNode
    * @param qtiName
    * @param qti
    * @return
    */
  def list(zip: ZipFile, resourceNode: Node, qtiName:String, qti: Node) : Seq[ManifestResource] = {

    val qtiResource = QtiManifestResource(qtiName, qti)
    val fileResources = getFileResources(zip, qtiName, resourceNode)

    /**
      * The node contents can be inside CDATA.
      * We want to look at whats inside, so we:
      * - convert qti to string
      * - strip CDATA
      * - fix markup
      * - parse again
      * - locate resources.
      */
    val flattenedQti = Flattener.flatten(qti)
    val qtiResources : Seq[ManifestResource] = resourcesFromNode(flattenedQti)

    val passagePaths = fileResources.filter(_.is(ManifestResourceType.Passage)).map(_.path)

    val passageResources = passagePaths.flatMap{ p =>
      ZipReader.xml(zip, p).map{ xml =>
        val flattened = Flattener.flatten(xml)
        resourcesFromNode(flattened)
      }
    }.flatten

    logger.debug(describe(qtiResource))
    logger.debug(describe(fileResources))
    logger.debug(describe(passageResources))
    (qtiResource +: (fileResources ++ qtiResources ++ passageResources)).distinct
  }

  /**
    * Resources in <resource><file href=""></file>...</resource>
    * @param qtiName
    * @param node
    * @return
    */
  private def getFileResources(zip: ZipFile, qtiName:String, node: Node) = {
    val files = (node \\ "file")

    val stripped = files.filter(n => (n \ "@href").text.toString != qtiName)

    stripped.flatMap{ n =>
      val path = (n \ "@href").text.toString
      val resourceType = ManifestResourceType.fromPath(path, n)
      if(resourceType == ManifestResourceType.Passage){
        val out = ZipReader.xml(zip, path).map( xml => PassageManifestResource(path, xml))

        if(out.isEmpty){
          logger.warn(s"cant load passage: $path")
        }
        out

      } else {
        Some(ManifestResource(
          path,
          resourceType,
          inline = resourceType == ManifestResourceType.StyleSheet))
      }
    }
  }

  private def flattenPath(path: String) = {
    var regexes = Seq(
      ("""(\.\.\/)+(.*)""".r, "$2"),
      ("""\.\/(.*)""".r, "$1")
    )
    regexes.foldLeft(path)((acc, r) => r._1.replaceAllIn(acc, r._2))
  }

  protected def resourcesFromNode(node:Node) : Seq[ManifestResource] = {

    val m = resourceLocators.map { case (resourceType, fn) => resourceType -> fn(node) }

    val out = m.flatMap{
      case(resourceType, filenames) => {

        filenames.map{ filename =>

          //TODO: what's this about??
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
        }
      }
    }
    out.toSeq
  }

}
