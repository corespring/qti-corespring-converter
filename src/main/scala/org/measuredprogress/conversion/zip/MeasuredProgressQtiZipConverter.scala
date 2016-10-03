package org.measuredprogress.conversion.zip

import java.util.zip.ZipFile

import com.keydatasys.conversion.qti.{ItemTransformer}
import com.keydatasys.conversion.zip.KDSQtiZipConverter._
import org.corespring.common.file.SourceWrapper
import org.corespring.common.util.UnicodeCleaner
import org.corespring.conversion.zip.QtiToCorespringConverter
import org.measuredprogress.conversion.qti.{ItemExtractor, QtiTransformer}
import play.api.libs.json._

import scala.collection.JavaConversions._
import scala.io.Source
import scalaz.{Failure, Success, Validation}

object MeasuredProgressQtiZipConverter extends QtiToCorespringConverter with UnicodeCleaner {

//  val collectionId = "57dac29977c896d6a7cafac4"
//  val collectionId = "57daeaece4b00d6de0ff5f35" // all but until now
  val collectionId = "57eeb030e4b01a332ddfc0f9"
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
    writeZip(toZipByteArray(processedFiles), path)
  }

}
