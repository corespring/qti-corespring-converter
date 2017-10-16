package org.measuredprogress.conversion.zip

import java.nio.file.Files
import java.util.zip.ZipFile

import com.keydatasys.conversion.qti.ItemTransformer
import com.keydatasys.conversion.zip.KDSQtiZipConverter._
import org.apache.commons.io.IOUtils
import org.corespring.common.CorespringItem
import org.corespring.common.file.SourceWrapper
import org.corespring.common.util.UnicodeCleaner
import org.corespring.conversion.qti.manifest.{ManifestItem, ZipReader}
import org.corespring.conversion.zip.{ConversionOpts, QtiToCorespringConverter}
import org.measuredprogress.conversion.qti.{ItemExtractor, QtiTransformer => MeasuredProgressQtiTransformer}
import play.api.libs.json._

import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.{Failure, Success, Validation}
import org.corespring.macros.DescribeMacro._

import scala.xml.Node

object MeasuredProgressQtiZipConverter extends QtiToCorespringConverter with UnicodeCleaner {

  val collectionId = "57eeb030e4b01a332ddfc0f9"
  val collectionName = "Measured Progress"

  override def convert(

                        zip: ZipFile,
                        output: String = "target/corespring-json.zip",
                        metadata: Option[JsObject] = None,
                        opts : ConversionOpts = ConversionOpts()): Future[ZipFile] = Future{

    logger.info(describe(output, opts))

    val tmpDir = Files.createTempDirectory("qti-conversion")
    logger.debug(describe(tmpDir))

    val manifestEntry = zip.getEntry("imsmanifest.xml")
    val is = zip.getInputStream(manifestEntry)
    val xml = filterManifest(SourceWrapper("imsmanifest.xml", is))

    val (qtiResources, resources) = (xml \ "resources" \\ "resource")
      .partition(r => (r \ "@type").text.toString == "imsqti_item_xmlv2p1")


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
          val playerDefinition = ItemTransformer.transform(scrubbed, m, sources)
          sources.mapValues { v =>
            IOUtils.closeQuietly(v.inputStream)
          }

          //          logger.trace(describe(playerDefinition))

          val id = "(.*).xml".r.replaceAllIn(m.filename, "$1")
          val metadata = maybeMetadata.getOrElse(obj()) ++ MetadataExtractor.sourceIdObj(id)

          //set a default title
          val title = (metadata \ "scoringType").asOpt[String].map{ st =>
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
            None
          }
        }
    /*val fileMap = zip.entries.filterNot(_.isDirectory).map(entry => {
      entry.getName -> SourceWrapper(entry.getName, zip.getInputStream(entry))
    }).toMap

    //TODO: linking to kds ItemTransformer - should be a common transformer?
    val extractor = new ItemExtractor(fileMap, metadata.getOrElse(Json.obj()), new ItemTransformer(MeasuredProgressQtiTransformer))

    val itemCount = extractor.ids.length

    val processedFiles = extractor.ids.zipWithIndex.map{ case(id, index) => {
      println(s"Processing ${id} (${index+1}/$itemCount)")
      val itemJson = extractor.itemJson
      val meta = extractor.meta
      val result: Validation[Error, (JsValue, JsObject)] = (itemJson.get(id).getOrElse(Failure(new Error("Missing item JSON"))),
        meta.get(id)) match {
          case (Failure(error), _) => Failure(error)
          case (_, None) => Failure(new Error("Missing item metadata"))
          case (Success(itemJson), Some(md)) => {
            Success((postProcess(itemJson), md.getOrElse(Json.obj())))
          }
        }
      result match {
        case Success((json, metadata)) => {
          val content = Json.obj()
          val basePath = s"${collectionName}_${collectionId}/${extractor.measuredProgressId.get(id).getOrElse(throw new Exception("Could not find id"))}"
          Seq(s"$basePath/player-definition.json" -> Source.fromString(Json.prettyPrint(json)),
            s"$basePath/profile.json" -> Source.fromString(Json.prettyPrint(
              content.deepMerge(metadata)))) ++
            extractor.filesFromManifest(id).map(filename => s"$basePath/data/${filename.flattenPath}" -> fileMap.get(filename))
              .filter { case (filename, maybeSource) => maybeSource.nonEmpty }
              .map { case (filename, someSource) => (filename, someSource.get.toSource()) }
        }
        case _ => Seq.empty[(String, Source)]
      }
    }}.flatten.toMap
    writeZip(toZipByteArray(processedFiles), path)*/
  }

}
