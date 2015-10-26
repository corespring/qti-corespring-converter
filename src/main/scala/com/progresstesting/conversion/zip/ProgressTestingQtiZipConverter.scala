package com.progresstesting.conversion.zip

import java.io.{FileOutputStream, File, ByteArrayOutputStream}
import java.util.zip.{ZipEntry, ZipOutputStream, ZipFile}

import com.keydatasys.conversion.qti.ItemExtractor
import com.keydatasys.conversion.zip.KDSQtiZipConverter._
import org.corespring.common.file.SourceWrapper
import org.corespring.conversion.zip.QtiToCorespringConverter
import play.api.libs.json.{JsValue, Json, JsObject}

import scala.collection.JavaConversions._
import scala.io.Source
import scalaz.{Success, Failure, Validation}

object ProgressTestingQtiZipConverter extends QtiToCorespringConverter {

  private val collectionName = "progresstesting"
  private val collectionId = "56292ddce4b0bcb3ef4221f4"

  override def convert(zip: ZipFile, path: String = "target/corespring-json.zip", metadata: Option[JsObject] = None): ZipFile = {

    val fileMap = zip.entries.filterNot(_.isDirectory).map(entry => {
      entry.getName -> SourceWrapper(entry.getName, zip.getInputStream(entry))
    }).toMap

    val extractor = new ItemExtractor(fileMap, metadata.getOrElse(Json.obj()))
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
          Success((itemJson, taskInfo))
        }
      }
      result match {
        case Success((json, taskInfo)) => {
          val basePath = s"${collectionName}_${collectionId}/$id"
          Seq(s"$basePath/player-definition.json" -> Source.fromString(Json.prettyPrint(json)),
            s"$basePath/profile.json" -> Source.fromString(Json.prettyPrint(
              Json.obj("taskInfo" -> taskInfo, "originId" -> id)))) ++
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
      "relatedSubject" -> Some(Json.arr()),
      "domains" -> Some(Json.arr()),
      "extended" -> metadata.map(md => Json.obj(
        "kds" -> md
      ))
    )
  }

  private def writeZip(byteArray: Array[Byte], path: String) = {
    val file = new File(path)
    val fileOutput = new FileOutputStream(file)
    try {
      fileOutput.write(byteArray)
    } finally {
      fileOutput.close()
    }
    new ZipFile(file)
  }

  private def toZipByteArray(files: Map[String, Source]) = {
    val bos = new ByteArrayOutputStream()
    val zipFile = new ZipOutputStream(bos)
    files.foreach{ case (filename, contents) => {
      zipFile.putNextEntry(new ZipEntry(filename))
      zipFile.write(contents.map(_.toByte).toArray)
    }}
    zipFile.close
    bos.toByteArray
  }


}
