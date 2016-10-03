package com.progresstesting.conversion.zip

import java.io.{FileOutputStream, File, ByteArrayOutputStream}
import java.util.zip.{ZipEntry, ZipOutputStream, ZipFile}

import com.keydatasys.conversion.qti.{ItemTransformer, ItemExtractor}
import com.keydatasys.conversion.zip.KDSQtiZipConverter._
import org.corespring.common.file.SourceWrapper
import org.corespring.common.util.{UnicodeCleaner, Rewriter}
import org.corespring.conversion.qti.QtiTransformer
import org.corespring.conversion.zip.QtiToCorespringConverter
import play.api.libs.json.{JsString, JsValue, Json, JsObject}

import scala.collection.JavaConversions._
import scala.io.Source
import scalaz.{Success, Failure, Validation}

object ProgressTestingQtiZipConverter extends QtiToCorespringConverter with UnicodeCleaner {

  private val collectionName = "progress-testing"
  private val collectionId = "5665af0ce4b03794c324adbd"

  override def convert(zip: ZipFile, path: String = "target/corespring-json.zip", metadata: Option[JsObject] = None): ZipFile = {

    val fileMap = zip.entries.filterNot(_.isDirectory).map(entry => {
      entry.getName -> SourceWrapper(entry.getName, zip.getInputStream(entry))
    }).toMap

    val extractor = new ItemExtractor(fileMap, metadata.getOrElse(Json.obj()), new ItemTransformer(QtiTransformer))
    val itemCount = extractor.ids.length
    val processedFiles = extractor.ids.zipWithIndex.map{ case(id, index) => {
      println(s"Processing ${id} (${index+1}/$itemCount)")
      val itemJson = extractor.itemJson
      val meta = extractor.metadata
      val result: Validation[Error, (JsValue, JsObject
        )] = (itemJson.get(id).getOrElse(Failure(new Error("Missing item JSON"))),
        meta.get(id).getOrElse(Failure(new Error("Missing item metadata")))) match {
        case (Failure(error), _) => Failure(error)
        case (_, Failure(error)) => Failure(error)
        case (Success(itemJson), Success(md)) => {
          implicit val metadata = md
          Success((postProcess(itemJson), taskInfo))
        }
      }
      result match {
        case Success((json, taskInfo)) => {
          val basePath = s"${collectionName}_${collectionId}/$id"
          Seq(s"$basePath/player-definition.json" -> Source.fromString(Json.prettyPrint(json)),
            s"$basePath/profile.json" -> Source.fromString(Json.prettyPrint(
              partialObj("taskInfo" -> Some(taskInfo), "originId" -> Some(JsString(id)))))) ++
            extractor.filesFromManifest(id).map(filename => s"$basePath/data/${filename.flattenPath}" -> fileMap.get(filename))
              .filter { case (filename, maybeSource) => maybeSource.nonEmpty }
              .map { case (filename, someSource) => (filename, someSource.get.toSource()) }
        }
        case _ => Seq.empty[(String, Source)]
      }
    }}.flatten.toMap
    writeZip(toZipByteArray(processedFiles), path)
  }

  private def taskInfo(implicit metadata: Option[JsValue]): JsObject = {
    partialObj(
      "title" -> metadata.map(md => JsString((md \ "sourceId").as[String])),
      "relatedSubject" -> Some(Json.arr()),
      "domains" -> Some(Json.arr()),
      "extended" -> metadata.map(md => Json.obj(
        "progresstesting" -> md
      ))
    )
  }

}
