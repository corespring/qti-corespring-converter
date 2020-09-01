package org.corespring.conversion.qti.manifest

import java.io.{File, FileInputStream, FileOutputStream}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import com.keydatasys.conversion.qti.util.PassageScrubber
import org.apache.commons.io.IOUtils
import org.corespring.common.util.EntityEscaper
import org.slf4j.LoggerFactory
import org.corespring.macros.DescribeMacro.describe

import scala.xml.{Node, XML}

object ZipWriter {
   def compressDir(src: File,  outputFile:String) = {
    val  zipFile = new ZipOutputStream(new FileOutputStream(outputFile))
    compressDirectoryToZipfile(src, src, zipFile)
    IOUtils.closeQuietly(zipFile)
  }

  private def compressDirectoryToZipfile(
                                          rootDir:File,
                                          sourceDir:File,
                                          out: ZipOutputStream) : Unit = {
    sourceDir.listFiles().map{ file =>
      if (file.isDirectory()) {
        compressDirectoryToZipfile(rootDir, new File(file.getAbsolutePath()), out)
      } else {
        val name = file.getCanonicalPath().replace(s"${rootDir.getCanonicalPath()}/", "")
        val entry = new ZipEntry(name)
        out.putNextEntry(entry)
        val in = new FileInputStream(file)
        IOUtils.copy(in, out)
        IOUtils.closeQuietly(in)
      }
    }
  }
}

object ZipReader extends PassageScrubber with EntityEscaper {

  lazy val logger = LoggerFactory.getLogger(ZipReader.this.getClass)

  private def stripCDataTags(xmlString: String) =
    """(?s)<!\[CDATA\[(.*?)\]\]>""".r.replaceAllIn(xmlString, "$1")



  def fileContents(zip: ZipFile, name:String) : Option[String] = {
    val entry = zip.getEntry(name)
    if(entry == null){
      None
    } else {
      val is = zip.getInputStream(entry)
      val s = IOUtils.toString(is, "UTF-8")
      IOUtils.closeQuietly(is)
      Some(s)
    }
  }

  def fileXML(zip: ZipFile, name:String) : Option[Node] = fileContents(zip, name)
    .flatMap{ s =>
      try {
        val xmlOut = XML.loadString(scrub(escapeEntities(stripCDataTags(s))))
        Some(xmlOut)
      }
      catch {
        case e :Exception => {
          logger.error(s"Error reading $name")
          if(logger.isDebugEnabled()){
            //e.printStackTrace()
          }
          None
        }
      }
    }
}
case class ManifestItem(
                         id: String,
                         filename: String,
                         resources: Seq[ManifestResource] = Seq.empty, manifest: Node)

/**
  * TODO: We are locating stylesheets in 2 possible places:
  *
  * 1. in the qti (this was added by ed to make sure measured progress picked up the stylesheet)
  *   aka: <stylesheet href="blah"></stylesheet>
  * 2. in the resource node:
  *   aka: <resource><file href=""></file></resource>
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

object ManifestItem extends PassageScrubber with EntityEscaper{

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


  def apply(node: Node, zip : ZipFile) : ManifestItem = {
    ManifestItem(node, zip, _ => None)
  }

  def apply(node: Node, zip : ZipFile, getId: Node => Option[String] ) : ManifestItem = {

    val qtiFile = (node \ "@href").text.toString
    val files = ZipReader.fileXML(zip, qtiFile).map(qti => {
      logger.trace(s"node: ${node.toString}")
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
            isInline = resourceType == ManifestResourceType.StyleSheet
          )

          logger.debug(describe(out))
          out
        })
      }
    }.toSeq

    logger.debug(s"files: ${files}")

    val resources = ((node \\ "file")
      .filterNot(f => (f \ "@href").text.toString == qtiFile).map(f => {
      val path = (f \ "@href").text.toString
      val resourceType = ManifestResourceType.fromPath(path, f)
      ManifestResource(
        path,
        resourceType,
        isInline = resourceType == ManifestResourceType.StyleSheet)

    })) ++ files :+ ManifestResource(
      qtiFile,
      ManifestResourceType.QTI,
      isInline = false)

    logger.debug(s"resources: $resources")

    val unLoadedPassageResources: Seq[ManifestResource] = resources.filter(resource => resource.is(ManifestResourceType.Passage) || resource.is(ManifestResourceType.QTI))

    def toXml(mr : ManifestResource) : Option[Node] = ZipReader.fileXML(zip, mr.path)

    val passageResources = unLoadedPassageResources
      .flatMap(toXml)
      .flatMap(xml => resourceLocators.map {
        case (resourceType, fn) => (resourceType, fn(xml))
      })
      .flatMap{
        case (resourceType, paths) =>
          paths.map{ path =>
            ManifestResource(
              path = flattenPath(path),
              resourceType = resourceType,
              isInline = resourceType == ManifestResourceType.StyleSheet)
          }
      }


    logger.debug(describe(passageResources))

    val missingPassageResources = passageResources.map(_.path).filter(path => zip.getEntry(path) == null)

    if (missingPassageResources.nonEmpty) {
      missingPassageResources.foreach(f => logger.warn(s"Missing file $f in uploaded import"))
    }

    val out = (resources ++ passageResources).distinct
    logger.debug(s"out: $out")
    val id = getId(node).getOrElse((node \ "@identifier").text.toString)

    ManifestItem( id, filename = qtiFile, resources = out, node)
  }
}
