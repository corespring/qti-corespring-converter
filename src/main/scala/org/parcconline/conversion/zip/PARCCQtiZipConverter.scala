package org.parcconline.conversion.zip

import java.io._
import java.util.zip._

import com.keydatasys.conversion.qti.{ItemTransformer, ItemExtractor}
import com.keydatasys.conversion.zip.KDSQtiZipConverter._
import com.keydatasys.conversion.zip.KDSQtiZipConverter.collectionId
import com.keydatasys.conversion.zip.KDSQtiZipConverter.collectionName
import com.keydatasys.conversion.zip.KDSQtiZipConverter.postProcess
import com.keydatasys.conversion.zip.KDSQtiZipConverter.taskInfo
import com.keydatasys.conversion.zip.KDSQtiZipConverter.unescapeCss
import com.progresstesting.conversion.util.UnicodeCleaner
import com.progresstesting.conversion.zip.ProgressTestingQtiZipConverter._
import org.apache.commons.io.IOUtils
import org.corespring.common.file.SourceWrapper
import org.corespring.common.util.Rewriter
import org.corespring.conversion.qti.QtiTransformer
import org.corespring.conversion.zip.QtiToCorespringConverter
import play.api.libs.json.{JsString, JsValue, Json, JsObject}

import scala.collection.JavaConversions._
import scala.io.Source
import scalaz.{Success, Failure, Validation}

object PARCCQtiZipConverter extends QtiToCorespringConverter with UnicodeCleaner {

  private val collectionName = "parcc"
  private val collectionId = "56d89830e4b024dcd2e5a568"

  def handleZip(zip: ZipFile, entryName: String, outputStream: ZipOutputStream) = {
    var tempFile: File = null
    var tempOut: FileOutputStream = null
    var innerZipFile: ZipFile = null

    try {
      tempFile = File.createTempFile("tempFile", "zip")
      tempOut = new FileOutputStream(tempFile)
      IOUtils.copy(zip.getInputStream(new ZipEntry(entryName)), tempOut)
      innerZipFile = new ZipFile(tempFile)
      processZip(innerZipFile, outputStream)
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      try {
        IOUtils.closeQuietly(tempOut)
        if (tempFile != null && !tempFile.delete()) {
          System.out.println("Could not delete " + tempFile)
        }
        try {
          if (innerZipFile != null)
            innerZipFile.close()
        } catch {
          case e: Exception => e.printStackTrace()
        }
      }
    }
  }

  def processZip(zip: ZipFile, outputStream: ZipOutputStream, metadata: Option[JsObject] = None) = {
    val fileMap = zip.entries.filterNot(_.isDirectory).map(entry => {
      entry.getName -> SourceWrapper(entry.getName, zip.getInputStream(entry))
    }).toMap

    val extractor = new ItemExtractor(fileMap, metadata.getOrElse(Json.obj()), new ItemTransformer(QtiTransformer))
    val itemCount = extractor.ids.length
    val processedFiles = extractor.ids.zipWithIndex.map{ case(id, index) => {
      //println(s"Processing ${id} (${index+1}/$itemCount)")
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
          val basePath = s"${collectionName}_${collectionId}/${id}_$index"
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

    processedFiles.foreach{ case (filename, contents) => {
      outputStream.putNextEntry(new ZipEntry(filename))
      outputStream.write(contents.map(_.toByte).toArray)
    }}

  }

  private def taskInfo(implicit metadata: Option[JsValue]): JsObject = {
    partialObj(
      "title" -> metadata.map(md => JsString((md \ "sourceId").as[String])),
      "relatedSubject" -> Some(Json.arr()),
      "domains" -> Some(Json.arr()),
      "extended" -> metadata.map(md => Json.obj(
        "parcc" -> md
      ))
    )
  }

  private def postProcess(item: JsValue): JsValue = item match {
    case json: JsObject => {
      val xhtml = unescapeCss(postprocessHtml((json \ "xhtml").as[String]))
      cleanUnicode(json ++ Json.obj(
        "xhtml" -> xhtml,
        "components" -> postprocessHtml((json \ "components")),
        "summaryFeedback" -> postprocessHtml((json \ "summaryFeedback").asOpt[String].getOrElse(""))
      ))
    }
    case _ => item
  }


  /**
   * Scala's XML parser won't even preserve these characters in CDATA tags.
   */
  private def unescapeCss(string: String): String = new Rewriter("""<style type="text/css">(.*?)</style>""") {
    def replacement() = s"""<style type="text/css">${group(1).replaceAll("&gt;", ">")}</style>"""
  }.rewrite(string)

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

  override def convert(zip: ZipFile, path: String = "target/corespring-json.zip", metadata: Option[JsObject]): ZipFile = {
    val bos = new ByteArrayOutputStream()
    val zipOutputStream = new ZipOutputStream(bos)
    new JEnumerationWrapper(zip.entries).filter(_.getName.endsWith("zip")).map(_.getName).toSeq.foreach(entryName =>
      handleZip(zip, entryName, zipOutputStream)
    )
    writeZip(bos.toByteArray, path)
  }

}