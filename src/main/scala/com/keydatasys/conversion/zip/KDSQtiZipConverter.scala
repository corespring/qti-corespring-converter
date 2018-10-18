package com.keydatasys.conversion.zip

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.zip.ZipFile

import com.keydatasys.conversion.qti.manifest.ManifestFilter
import com.keydatasys.conversion.qti.{KDSItemTransformer, KDSMode, KDSQtiTransformer, MetadataExtractor}
import com.keydatasys.conversion.qti.util.{PassageScrubber, PathFlattener}
import org.apache.commons.io.IOUtils
import org.corespring.common.CorespringItem
import org.corespring.common.file.SourceWrapper
import org.corespring.common.json.JsonUtil
import org.corespring.common.util.HtmlProcessor
import org.corespring.conversion.qti.manifest.{ManifestItem, ZipReader, ZipWriter}
import org.corespring.conversion.zip.{ConversionOpts, QtiToCorespringConverter}
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.libs.json.Json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.Node
import org.corespring.macros.DescribeMacro._

object KDSQtiZipConverter
  extends QtiToCorespringConverter
    with PathFlattener
    with HtmlProcessor
    with JsonUtil
    with PassageScrubber
    with ManifestFilter {

  private val collectionName = "kds"
  private val collectionId = "5453b4e4e4b05f38dd6440a8"

  private val logger = LoggerFactory.getLogger(KDSQtiZipConverter.this.getClass)

  def convert(
               zip: ZipFile,
               output: String = "target/corespring-json.zip",
               maybeMetadata: Option[JsObject] = None,
               opts: ConversionOpts = ConversionOpts()): Future[ZipFile] = {

    val scoringType = maybeMetadata.flatMap( o => (o \ "scoringType").asOpt[String]).getOrElse("PARCC")
    val mode = KDSMode.withName(scoringType)
    val transformer = new KDSItemTransformer(new KDSQtiTransformer(mode))

    logger.info(describe(output, opts))

    val tmpDir = Files.createTempDirectory("qti-conversion")
    logger.debug(describe(tmpDir))

    val manifestEntry = zip.getEntry("imsmanifest.xml")
    val is = zip.getInputStream(manifestEntry)
    val xml = filterManifest(SourceWrapper("imsmanifest.xml", is))

    val resources = (xml \ "resources" \\ "resource")
    if(resources.length == 0){
      logger.error("resources length is 0!")
    }
    val (qtiResources, _) = resources
      .partition(r => (r \ "@type").text.toString == "imsqti_item_xmlv2p1")


    if(qtiResources.length == 0){
      logger.error("qtiResources length is 0")
    }

    def toManifestItem(node: Node): Future[ManifestItem] = Future {
      val out = ManifestItem(node, zip)
      logger.info(describe(out))
      out
    }

    def toCorespringItem(m: ManifestItem): Future[Option[CorespringItem]] = Future {
      val qti = ZipReader.fileContents(zip, m.filename)

      qti.map { q =>
        try {
          logger.debug(describe(q))
          val preprocessed = preprocessHtml(q)
          logger.debug(describe(preprocessed))
          val scrubbed = scrub(preprocessed)
          logger.debug(describe(scrubbed))
          val sources: Map[String, SourceWrapper] = m.resources.toSourceMap(zip)
          val playerDefinition = transformer.transform(scrubbed, m, sources)
          sources.mapValues { v =>
            IOUtils.closeQuietly(v.inputStream)
          }

          //          logger.trace(describe(playerDefinition))

          val id = "(.*).xml".r.replaceAllIn(m.filename, "$1")
          val metadata = maybeMetadata.getOrElse(obj()) ++ MetadataExtractor.sourceIdObj(id)

          //set a default title
          val title = (metadata \ "scoringType").asOpt[String].map { st =>
            s"${m.id} - $st"
          }.getOrElse(m.id)

          val profile = obj("taskInfo" -> obj(
            "title" -> title,
            "description" -> title,
            "extended" -> obj(
              "kds" -> metadata
            )
          ))

          val out = CorespringItem(m.id, postProcess(playerDefinition), profile, m.resources.map(_.path))
          logger.trace(describe(id, out))
          Some(out)
        } catch {
          case e: Exception => {
            logger.warn(e.getMessage)
//            e.printStackTrace()
            None
          }
        }
      }.flatten
    }

    def writeCorespringItem(item: Option[CorespringItem]): Future[Option[CorespringItem]] = item.map { i =>
      Future {
        val basePath = Paths.get(s"${collectionName}_$collectionId/${i.id}")
        val resolved = tmpDir.resolve(basePath)
        val dataPath = resolved.resolve(Paths.get("data"))

        logger.debug(s"[writeCorespringItem] resolved: $resolved")
        logger.debug(s"[writeCorespringItem] dataPath: $dataPath")

        if (Files.notExists(resolved)) {
          Files.createDirectories(resolved)
        }

        if (Files.notExists(dataPath)) {
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

        def flattenPath(p: String) = p.split("/").last

        i.assets.map {
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

    def convertResource(n: Node) = toManifestItem(n)
      .flatMap(mi => toCorespringItem(mi))
      .flatMap(ci => writeCorespringItem(ci))


    val nodes = {

      val n = if (opts.sourceIds.isEmpty) {
        qtiResources
      } else {
        qtiResources.filter { n =>
          val id = (n \ "@identifier").text.toString.trim
          opts.sourceIds.contains(id)
        }
      }

      if (opts.limit > 0) n.take(opts.limit) else n
    }

    logger.info(s"nodes length: ${nodes.length}")

    val futures = nodes.map(convertResource)

    logger.trace(s"futures: ${futures}")

    Future.sequence(futures)
      .map(results => {
        val ids = results.map(ci => ci.map(_.id).getOrElse("?"))
        logger.info(s"all resources have been written, no of items written: ${ids.length}, zipping...")
        ZipWriter.compressDir(tmpDir.toFile(), output)
        val outFile = new File(output)
        logger.info(s"zip complete ${outFile.getAbsolutePath}")
        new ZipFile(outFile)
      })
  }

  override def postProcess(item: JsValue): JsValue = item match {
    case json: JsObject => {
      json ++ obj(
        "xhtml" -> unescapeCss(postprocessHtml((json \ "xhtml").as[String])),
        "components" -> postprocessHtml((json \ "components")),
        "summaryFeedback" -> postprocessHtml((json \ "summaryFeedback").asOpt[String].getOrElse(""))
      )
    }
    case _ => item
  }


}
