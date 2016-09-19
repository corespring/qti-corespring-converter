package org.measuredprogress.conversion.zip

import java.util.zip.ZipFile

import com.keydatasys.conversion.qti.{ItemTransformer}
import com.keydatasys.conversion.zip.KDSQtiZipConverter._
import org.corespring.common.file.SourceWrapper
import org.corespring.common.util.UnicodeCleaner
import org.corespring.conversion.qti.ItemExtractor
import org.corespring.conversion.zip.QtiToCorespringConverter
import org.measuredprogress.conversion.qti.QtiTransformer
import play.api.libs.json._

import scala.collection.JavaConversions._
import scala.io.Source
import scalaz.{Failure, Success, Validation}

object MeasuredProgressQtiZipConverter extends QtiToCorespringConverter with UnicodeCleaner {

//  val collectionId = "57dac29977c896d6a7cafac4"
  val collectionId = "57daeaece4b00d6de0ff5f35"
  val collectionName = "Measured Progress"

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
          Success((postProcess(itemJson), taskInfo(id)))
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

  private def taskInfo(id: String)(implicit metadata: Option[JsValue]): JsObject = {
    partialObj(
      "relatedSubject" -> Some(Json.arr()),
      "domains" -> Some(Json.arr()),
      "title" -> Some(JsString(id)),
      "extended" -> metadata.map(md => Json.obj(
        "measuredprogress" -> md
      ))
    )
  }

}
