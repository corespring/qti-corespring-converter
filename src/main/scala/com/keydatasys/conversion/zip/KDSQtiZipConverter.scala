package com.keydatasys.conversion.zip

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.zip.ZipFile

import com.keydatasys.conversion.qti.manifest.ManifestFilter
import com.keydatasys.conversion.qti.util.PathFlattener
import com.keydatasys.conversion.qti.{ItemTransformer, MetadataExtractor}
import org.apache.commons.io.IOUtils
import org.corespring.common.CorespringItem
import org.corespring.common.json.JsonUtil
import org.corespring.conversion.qti.manifest.{ManifestItem, ZipWriter}
import org.corespring.conversion.zip.{ConversionOpts, QtiToCorespringConverter}
import org.corespring.macros.DescribeMacro._
import org.corespring.utils.ErrorDir
import org.slf4j.LoggerFactory
import play.api.libs.json.Json._
import play.api.libs.json.{JsObject, JsString, JsValue, Json}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.Node

object KDSQtiZipConverter
  extends QtiToCorespringConverter
    with PathFlattener
    with JsonUtil {

  private val collectionName = "kds"
  private val collectionId = "5453b4e4e4b05f38dd6440a8"

  private val logger = LoggerFactory.getLogger(KDSQtiZipConverter.this.getClass)

  def convert(
               zip: ZipFile,
               output: String = "target/corespring-json.zip",
               maybeMetadata: Option[JsObject] = None,
               opts: ConversionOpts = ConversionOpts()): Future[ZipFile] = {

    logger.info(describe(output, opts))

    val tmpDir = Files.createTempDirectory("qti-conversion")
    logger.debug(describe(tmpDir))

    val manifestEntry = zip.getEntry("imsmanifest.xml")
    val is = zip.getInputStream(manifestEntry)
    logger.debug(">> parse xml")
    val xml = ManifestFilter.filterManifest(is)

    logger.debug(">> parsed xml")

    val (qtiResources, resources) = (xml \ "resources" \\ "resource")
      .partition(r => (r \ "@type").text.toString == "imsqti_item_xmlv2p1")

    def toManifestItem(node: Node): Future[ManifestItem] = Future {
      val out = ManifestItem(node, zip)
      logger.trace(describe(out))
      out
    }

    def toCorespringItem(m: ManifestItem): Future[Option[CorespringItem]] = Future {

      m.qti.map { q =>
        try {
          //val sources: Map[String, SourceWrapper] = m.resources.toSourceMap(zip)
          val playerDefinition = ItemTransformer.transform(q.qti, m)
//          sources.mapValues { v =>
//            IOUtils.closeQuietly(v.inputStream)
//          }

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
            logger.error(s"Error reading ${m.filename}: ${e.getMessage}")

            ErrorDir.dump(m.id, Some(e), "qti.xml" -> m.qti.map(_.qti.toString()).get)
            if(logger.isDebugEnabled){
              e.printStackTrace()
            }
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
            if(entry == null){
              logger.warn(s"missing resource: $a for ${i.id}")
            } else {
              val is = zip.getInputStream(entry)
              val dest = dataPath.resolve(flattenPath(a))
              logger.debug(s"[writeCorespringItem] write $a to $dest")
              Files.copy(is, dest)
              IOUtils.closeQuietly(is)
            }
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
  //TODO: not a relevant name any more?
  def postProcess(item: JsValue): JsValue = item match {
    case json: JsObject => {
      val additions : JsObject = obj(
        "xhtml" -> (json \ "xhtml").as[String],
        "components" -> (json \ "components"),
        "summaryFeedback" -> JsString((json \ "summaryFeedback").asOpt[String].getOrElse(""))
      )
      (json ++ additions)
    }
    case _ => item
  }


}
