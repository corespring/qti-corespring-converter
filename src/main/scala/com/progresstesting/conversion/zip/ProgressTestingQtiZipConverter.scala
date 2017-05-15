package com.progresstesting.conversion.zip

import java.io.{ByteArrayOutputStream, File, FileOutputStream}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import com.keydatasys.conversion.qti.ItemTransformer
import com.keydatasys.conversion.qti.manifest.ManifestReader
import com.progresstesting.conversion.qti.ItemExtractor
import com.keydatasys.conversion.zip.KDSQtiZipConverter._
import org.corespring.common.file.SourceWrapper
import org.corespring.common.util.{Rewriter, UnicodeCleaner}
import org.corespring.conversion.qti.QtiTransformer
import org.corespring.conversion.qti.manifest.QTIManifest
import org.corespring.conversion.zip.{ConversionOpts, QtiToCorespringConverter}
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsObject, JsString, JsValue, Json}

import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.io.Source
import scalaz.{Failure, Success, Validation}

import scala.concurrent.ExecutionContext.Implicits.global

object ProgressTestingQtiZipConverter extends QtiToCorespringConverter with UnicodeCleaner {

  private val collectionName = "progress-testing"
  private val collectionId = "5665af0ce4b03794c324adbd"


  private val logger = LoggerFactory.getLogger("converter")
  override def convert(
                        zip: ZipFile,
                        path: String = "target/corespring-json.zip",
                        metadata: Option[JsObject] = None,
                      opts: ConversionOpts = ConversionOpts()): Future[ZipFile] = Future{


    val manifestXml = zip.getEntry("imsmanifest.xml")

    val fileMap = zip.entries.filterNot(_.isDirectory).map(entry => {
      entry.getName.flattenPath -> SourceWrapper(entry.getName, zip.getInputStream(entry))
    }).toMap

    //logger.info(s"fileMap: $fileMap")

    val extractor = new ItemExtractor(zip, fileMap, metadata.getOrElse(Json.obj()), new ItemTransformer(QtiTransformer))
    val itemCount = extractor.ids.length

    logger.trace(s"itemCount $itemCount")

    val processedFiles = extractor.ids.take(10).zipWithIndex.map{ case(id, index) => {
      logger.info(s"Processing ${id} (${index+1}/$itemCount)")
      val itemJson = extractor.itemJson
      val meta = extractor.metadata
      val result: Validation[Error, (JsValue, JsObject
        )] = (itemJson.get(id).getOrElse(Failure(new Error("Missing item JSON"))),
        meta.get(id).getOrElse(Failure(new Error("Missing item metadata")))) match {
        case (Failure(error), _) => Failure(error)
        case (_, Failure(error)) => Failure(error)
        case (Success(itemJson), Success(md)) => {
          implicit val metadata = md
          val profile = metadata match {
            case Some(js: JsObject) => js.deepMerge(Json.obj("taskInfo" -> taskInfo))
            case _ => Json.obj("taskInfo" -> taskInfo)
          }
          Success((postProcess(itemJson), profile))
        }
      }
      result match {
        case Success((json, profile)) => {
          val basePath = s"${collectionName}_${collectionId}/$id"
          Seq(s"$basePath/player-definition.json" -> Source.fromString(Json.prettyPrint(json)),
            s"$basePath/profile.json" -> Source.fromString(Json.prettyPrint(profile))) ++
            extractor.filesFromManifest(id).map(filename => s"$basePath/data/${filename.flattenPath}" -> fileMap.get(filename))
              .filter { case (filename, maybeSource) => { maybeSource.nonEmpty } }
              .map { case (filename, someSource) => (filename, someSource.get.toSource()) }
        }
        case _ => Seq.empty[(String, Source)]
      }
    }}.flatten.toMap
    writeZip(toZipByteArray(processedFiles), path)
  }

  private def taskInfo(implicit metadata: Option[JsValue]): JsObject = {
    partialObj(
      "relatedSubject" -> Some(Json.arr()),
      "domains" -> Some(Json.arr())
    )
  }

}
