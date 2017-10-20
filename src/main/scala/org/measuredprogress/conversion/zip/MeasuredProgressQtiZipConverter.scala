package org.measuredprogress.conversion.zip

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.zip.ZipFile

import com.keydatasys.conversion.qti.{ItemTransformer => KDSItemTransformer}
import com.keydatasys.conversion.zip.KDSQtiZipConverter.{filterManifest, scrub}
import org.apache.commons.io.IOUtils
import org.corespring.common.CorespringItem
import org.corespring.common.file.SourceWrapper
import org.corespring.common.util.UnicodeCleaner
import org.corespring.conversion.qti.manifest.{ManifestItem, ZipReader, ZipWriter}
import org.corespring.conversion.zip.{ConversionOpts, QtiToCorespringConverter}
import org.measuredprogress.conversion.qti.{MeasuredProgressExtractor, QtiTransformer => MPQtiTransformer}
import play.api.libs.json._
import play.api.libs.json.Json._

import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import org.corespring.macros.DescribeMacro._
import org.slf4j.LoggerFactory

import scala.xml.Node

object MeasuredProgressQtiZipConverter extends QtiToCorespringConverter with UnicodeCleaner {

  private val logger = LoggerFactory.getLogger(MeasuredProgressQtiZipConverter.this.getClass)

  val collectionId = "57eeb030e4b01a332ddfc0f9"
  val collectionName = "Measured Progress"

  override def convert(

                        zip: ZipFile,
                        output: String = "target/corespring-json.zip",
                        maybeMetadata: Option[JsObject] = None,
                        opts: ConversionOpts = ConversionOpts()): Future[ZipFile] = {

    logger.info(describe(output, opts))

    val tmpDir = Files.createTempDirectory("qti-conversion")
    logger.debug(describe(tmpDir))

    val manifestEntry = zip.getEntry("imsmanifest.xml")
    val is = zip.getInputStream(manifestEntry)
    val xml = filterManifest(SourceWrapper("imsmanifest.xml", is))

    val (qtiResources, _) = (xml \ "resources" \\ "resource")
      .partition(r => (r \ "@type").text.toString == "imsqti_item_xmlv2p1")


    def nodeToSourceId(node: Node) =
      MeasuredProgressExtractor.getId((node \ "@href").text.trim)

    def toManifestItem(node: Node): Future[ManifestItem] = Future {
      val out = ManifestItem(node, zip, n => Some(nodeToSourceId(n)))
      logger.trace(describe(out))
      out
    }

    def toCorespringItem(m: ManifestItem): Future[Option[CorespringItem]] = Future {

      logger.trace(zip.entries().map(_.getName).mkString("\n"))
      logger.debug(s"load xml from: ${m.filename}")

      val qti = ZipReader.fileContents(zip, m.filename)

      logger.trace(describe(qti))

      qti.flatMap { q =>
        try {
          val preprocessed = preprocessHtml(q)
          val scrubbed = scrub(preprocessed)
          val sources: Map[String, SourceWrapper] = m.resources.toSourceMap(zip)
          val playerDefinition = new KDSItemTransformer(MPQtiTransformer).transform(scrubbed, m, sources)
                    //val playerDefinition = KDSItemTransformer.transform(scrubbed, m, sources)

          sources.mapValues { v =>
            IOUtils.closeQuietly(v.inputStream)
          }

          logger.trace(describe(playerDefinition))

          val id = "(.*).xml".r.replaceAllIn(m.filename, "$1")

          val profile = MeasuredProgressExtractor.getProfileJson(m, maybeMetadata.getOrElse(obj()))
          val csResources = m.resources.filterNot(_.inline).map(_.path)

          logger.debug(describe(csResources))

          val out = CorespringItem(m.id, postProcess(playerDefinition), profile, csResources)
          logger.trace(describe(id, out))
          Some(out)
        } catch {
          case e: Exception => {
            logger.error(e.getMessage)
            if (logger.isDebugEnabled) {
              e.printStackTrace()
            }
            None
          }
        }
      }
    }


    def writeCorespringItem(item: Option[CorespringItem]): Future[Option[CorespringItem]] = item.map { i =>
      Future {
        val basePath = Paths.get(s"${collectionName}_$collectionId/${i.id}")
        val resolved = tmpDir.resolve(basePath)
        val dataPath = resolved.resolve(Paths.get("data"))
        logger.debug(i.id)
          logger.debug(describe(basePath, resolved, dataPath))

        if (Files.notExists(resolved)) {
          Files.createDirectories(resolved)
        }

        if (Files.notExists(dataPath)) {
          Files.createDirectory(dataPath)
        }

        val pd = Json.prettyPrint(i.playerDefinition)
        val pr = Json.prettyPrint(i.profile)

        val pdPath = resolved.resolve("player-definition.json")
        val profilePath = resolved.resolve("profile.json")

        if(pdPath.toString.contains("MP-226")) {
            logger.debug(describe(pdPath, profilePath))
          }

        Files.write(
          pdPath,
          pd.getBytes(StandardCharsets.UTF_8))

        Files.write(
          profilePath,
          pr.getBytes(StandardCharsets.UTF_8))

//        logger.debug(s"[writeCorespringItem] assets: length: ${i.assets.length} - ${i.assets}")

        def flattenPath(p: String) = p.split("/").last

        i.assets.map {
          a =>
            val entry = zip.getEntry(a)
            val is = zip.getInputStream(entry)
            val dest = dataPath.resolve(flattenPath(a))
//            logger.debug(s"[writeCorespringItem] write $a to $dest")
            Files.copy(is, dest)
            IOUtils.closeQuietly(is)
        }
        Some(i)
      }

    }.getOrElse(Future.successful(None))

    def convertResource(n: Node) = toManifestItem(n)
      .flatMap(mi => toCorespringItem(mi))
      .flatMap(ci => writeCorespringItem(ci))

    val nodes = {
      val n = if (opts.sourceIds.isEmpty) qtiResources else qtiResources.filter { n =>
        val id = nodeToSourceId(n)
        opts.sourceIds.contains(id)
      }

      if (opts.limit > 0) n.take(opts.limit) else n
    }

    val futures = nodes.map(convertResource)

    logger.trace(s"futures: ${futures}")

    val out: Future[ZipFile] = Future.sequence(futures)
      .map(results => {
        val ids = results.map(ci => ci.map(_.id).getOrElse("?"))
        logger.info(s"all resources have been written, no of items written: ${ids.length}, zipping...")
        logger.info(s"zipping tmp dir: $tmpDir to: $output")
        ZipWriter.compressDir(tmpDir.toFile(), output)
        val outFile = new File(output)
        logger.info(s"zip complete ${outFile.getAbsolutePath}")
        new ZipFile(outFile)
      })

    out
  }
}
