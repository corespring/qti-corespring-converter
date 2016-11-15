package com.keydatasys.conversion.zip

import java.io.{FileInputStream, FileOutputStream, File, ByteArrayOutputStream}
import java.util.zip.{ZipEntry, ZipOutputStream, ZipFile}

import com.keydatasys.conversion.audio.Mp3ToOgg
import com.keydatasys.conversion.qti.{ItemTransformer, ItemExtractor}
import com.keydatasys.conversion.qti.util.PathFlattener
import org.apache.commons.io.FileUtils
import org.corespring.common.file.SourceWrapper
import org.corespring.common.json.JsonUtil
import org.corespring.common.util.{HtmlProcessor, Rewriter}
import org.corespring.conversion.zip.QtiToCorespringConverter
import play.api.libs.json._

import scala.collection.JavaConversions._
import scala.io.Source
import scalaz.{Validation, Success, Failure}

object KDSQtiZipConverter extends QtiToCorespringConverter with PathFlattener with HtmlProcessor with JsonUtil {

  private val collectionName = "kds"
  private val collectionId = "5453b4e4e4b05f38dd6440a8"

  override def convert(zip: ZipFile, path: String = "target/corespring-json.zip", metadata: Option[JsObject] = None): ZipFile = {

    val fileMap = zip.entries.filterNot(_.isDirectory).map(entry => {
      entry.getName.flattenPath -> SourceWrapper(entry.getName, zip.getInputStream(entry))
    }).toMap

    val extractor = new ItemExtractor(fileMap, metadata.getOrElse(Json.obj()), ItemTransformer)
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
              Json.obj("taskInfo" -> taskInfo, "originId" -> id)))) ++
            extractor.filesFromManifest(id).map(filename => s"$basePath/data/${filename.flattenPath}" -> fileMap.get(filename))
              .filter { case (filename, maybeSource) => {
                maybeSource.nonEmpty }}
              .map { case (filename, source) => filename.split("\\.").lastOption match {
                case Some("mp3") => try {
                  Seq(filename -> source, filename.replaceAll("mp3", "ogg") -> convertMp3(filename, source))
                } catch {
                  case e: Exception => Seq.empty
                }
                case _ => Seq(filename -> source)
              }}.flatten
              .map { case (filename, someSource) => (filename, someSource.get.toSource()) }
        }
        case _ => Seq.empty[(String, Source)]
      }
    }}.flatten.toMap
    writeZip(toZipByteArray(processedFiles), path)
  }

  private lazy val tempFilePath = {
    val temp = File.createTempFile("temp-file-name", ".tmp")
    temp.deleteOnExit()
    temp.getAbsolutePath.substring(0, temp.getAbsolutePath.lastIndexOf(File.separator))
  }

  private def convertMp3(filename: String, source: Option[SourceWrapper]) = source.map{ source =>
    val mp3Bytes = source.toByteArray
    val fname = filename.split("/").lastOption.map(l => l.substring(0, l.indexOf("."))).getOrElse("temp")
    val mp3File = File.createTempFile(fname, ".mp3")
    FileUtils.writeByteArrayToFile(mp3File, mp3Bytes)
    mp3File.deleteOnExit()

    val oggFile = s"$tempFilePath/$fname.ogg"
    Mp3ToOgg.convert(mp3File, oggFile)
    new File(oggFile).deleteOnExit()
    SourceWrapper(filename.replaceAll("mp3", "ogg"), new FileInputStream(oggFile))
  }

  override def postProcess(item: JsValue): JsValue = item match {
    case json: JsObject => {
      json ++ Json.obj(
        "xhtml" -> unescapeCss(postprocessHtml((json \ "xhtml").as[String])),
        "components" -> postprocessHtml((json \ "components")),
        "summaryFeedback" -> postprocessHtml((json \ "summaryFeedback").asOpt[String].getOrElse(""))
      )
    }
    case _ => item
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

}
