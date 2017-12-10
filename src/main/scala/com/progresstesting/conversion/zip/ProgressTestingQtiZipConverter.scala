package com.progresstesting.conversion.zip

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.zip.ZipFile

import com.keydatasys.conversion.qti.ItemTransformer
import com.keydatasys.conversion.qti.manifest.ManifestFilter
import com.progresstesting.conversion.qti.MetadataExtractor
import org.apache.commons.io.IOUtils
import org.corespring.common.CorespringItem
import org.corespring.conversion.qti.{QtiTransformer => DefaultQtiTransformer}
import org.corespring.common.file.SourceWrapper
import org.corespring.conversion.qti.manifest._
import org.corespring.conversion.zip.{ConversionOpts, QtiToCorespringConverter}
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.Node

object ProgressTestingQtiZipConverter
  extends QtiToCorespringConverter
  with ManifestFilter {

  private val collectionName = "progress-testing"
  private val collectionId = "5665af0ce4b03794c324adbd"

  val itemTransformer = new ItemTransformer(DefaultQtiTransformer)

  val logger = LoggerFactory.getLogger(ProgressTestingQtiZipConverter.this.getClass)

  def convert(
               zip: ZipFile,
               output: String = "target/corespring-json.zip",
               metadata: Option[JsObject] = None,
               opts: ConversionOpts = ConversionOpts()): Future[ZipFile] = {

    logger.info(s"output: $output, opts: $opts")

    val tmpDir = Files.createTempDirectory("qti-conversion")
    logger.debug(s"Created temp dir: $tmpDir")

    val manifestEntry = zip.getEntry("imsmanifest.xml")
    val is = zip.getInputStream(manifestEntry)
    val xml = filterManifest(SourceWrapper("imsmanifest.xml", is))

    val (qtiResources, resources) = (xml \ "resources" \\ "resource")
      .partition(r => (r \ "@type").text.toString == "imsqti_item_xmlv2p1")


    def toManifestItem(node: Node) : Future[ManifestItem] = Future{
      val out = ManifestItem(node, zip)
      logger.info(s"[toManifestItem] converted ${out.id}")
      out
    }

    def toCorespringItem(m:ManifestItem) : Future[Option[CorespringItem]] = Future{
      ???
//      val qti = ZipReader.fileContents(zip, m.filename)
//
//      qti.map{ q =>
//        val sources : Map[String, SourceWrapper] = m.resources.toSourceMap(zip)
//        val playerDefinition = itemTransformer.transform(q, m, sources)
//        sources.mapValues{ v =>
//          IOUtils.closeQuietly(v.inputStream )
//        }
//
//        val id = "(.*).xml".r.replaceAllIn(m.filename, "$1")
//        val common = metadata.getOrElse(Json.obj())
//        val resourceMetadata = MetadataExtractor.metadataFromResourceNode(m.manifest, id)
//        val profile = common ++ resourceMetadata
//        val out = CorespringItem(m.id, playerDefinition, profile, m.resources.map(_.path))
//        logger.info(s"[toCorespringItem] id: ${m.id}")
//        out
//      }
    }

    def writeCorespringItem(item: Option[CorespringItem]) : Future[Option[CorespringItem]] = item.map{ i =>
      Future {
        val basePath = Paths.get(s"${collectionName}_$collectionId/${i.id}")
        val resolved = tmpDir.resolve(basePath)
        val dataPath = resolved.resolve(Paths.get("data"))

        logger.debug(s"[writeCorespringItem] resolved: $resolved")
        logger.debug(s"[writeCorespringItem] dataPath: $dataPath")

        if(Files.notExists(resolved)){
          Files.createDirectories(resolved)
        }

        if(Files.notExists(dataPath)){
          Files.createDirectory(dataPath)
        }

        val pd = Json.prettyPrint(i.playerDefinition)
        val pr = Json.prettyPrint(i.profile)

        Files.write(
          resolved.resolve("player-definition.json"),
          pd.getBytes(StandardCharsets.UTF_8))

        Files.write(
          resolved.resolve("profile.json"),
          pr.getBytes(StandardCharsets.UTF_8))

        logger.debug(s"[writeCorespringItem] assets: length: ${i.assets.length} - ${i.assets}")

        def flattenPath(p:String) = p.split("/").last

        i.assets.map{
          a =>
            val entry = zip.getEntry(a)
            val is = zip.getInputStream(entry)
            val dest = dataPath.resolve(flattenPath(a))
            logger.debug(s"[writeCorespringItem] write $a to $dest")
            Files.copy(is, dest)
            IOUtils.closeQuietly(is)
        }
        Some(i)
      }

    }.getOrElse(Future.successful(None))

    def convertResource(n:Node) = toManifestItem(n)
      .flatMap( mi => toCorespringItem(mi) )
      .flatMap(ci => writeCorespringItem(ci))

    val futures = if (opts.limit != 0) {
      qtiResources.take(opts.limit).map(convertResource)
    } else {
      qtiResources.map(convertResource)
    }

    logger.trace(s"futures: ${futures}")

    Future.sequence(futures)
      .map( results => {
        val ids = results.map(ci =>ci.map(_.id).getOrElse("?"))
        logger.info(s"all resources have been written, no of items written: ${ids.length}, zipping...")
        ZipWriter.compressDir(tmpDir.toFile(), output)
        val outFile = new File(output)
        logger.info(s"zip complete ${outFile.getAbsolutePath}")
        new ZipFile(outFile)
      })
  }
}
