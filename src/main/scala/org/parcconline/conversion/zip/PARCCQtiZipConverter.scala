package org.parcconline.conversion.zip

import java.io._
import java.util.zip._

import com.keydatasys.conversion.qti.ItemTransformer
import org.parcconline.conversion.BootstrapTranslator
import org.parcconline.conversion.qti.{QtiTransformer, ItemExtractor}
import com.keydatasys.conversion.zip.KDSQtiZipConverter._
import com.progresstesting.conversion.util.UnicodeCleaner
import org.corespring.common.file.SourceWrapper
import org.corespring.common.util.Rewriter
import org.corespring.conversion.zip.QtiToCorespringConverter
import play.api.libs.json.{JsString, JsValue, Json, JsObject}

import scala.collection.JavaConversions._
import scala.io.Source
import scalaz.{Success, Failure, Validation}

object PARCCQtiZipConverter extends QtiToCorespringConverter with UnicodeCleaner {

  private val collectionName = "parcc"
  private val collectionId = "56d89830e4b024dcd2e5a568"

  object Subject {
    val Math = "4ffb535f6bb41e469c0bf2c2"
    val ELA = "4ffb535f6bb41e469c0bf2ad"
  }

  private def subjectFromMetadata(metadata: Option[JsObject]) = {
    metadata match {
      case Some(metadata) => (metadata \ "subject").asOpt[String] match {
        case Some("Math") => Subject.Math
        case Some(_) => Subject.ELA
        case _ => throw new Exception("Metadata must contain subject")
      }
      case _ => throw new Exception("Metadata w/ subject required for PARCC")
    }
  }

  override def convert(zip: ZipFile, path: String = "target/corespring-json.zip", metadata: Option[JsObject] = None): ZipFile = {
    val subject = subjectFromMetadata(metadata)
    val fileMap = zip.entries.filterNot(_.isDirectory).map(entry => {
      entry.getName -> SourceWrapper(entry.getName, zip.getInputStream(entry))
    }).toMap

    val extractor = new ItemExtractor(fileMap, metadata.getOrElse(Json.obj()), new ItemTransformer(new QtiTransformer(fileMap)))
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
          Success((postProcess(itemJson), taskInfo(zip.getName, subject)))
        }
      }
      result match {
        case Success((json, taskInfo)) => {
          val basePath = s"${collectionName}_${collectionId}/$id"
          val files = extractor.filesFromManifest(id)

          Seq(s"$basePath/player-definition.json" -> Source.fromString(Json.prettyPrint(json)),
            s"$basePath/profile.json" -> Source.fromString(Json.prettyPrint(
              Json.obj("taskInfo" -> taskInfo, "originId" -> id)))) ++
            extractor.filesFromManifest(id).map(filename => s"$basePath/data/${filename.flattenPath}" -> fileMap.keys.find(key => key.endsWith(filename)).map(key => fileMap.get(key).getOrElse(throw new Exception("Wat?!"))))
              .filter { case (filename, maybeSource) => maybeSource.nonEmpty }
              .map { case (filename, someSource) => (filename, someSource.get.toSource()) }
        }
        case _ => Seq.empty[(String, Source)]
      }
    }}.flatten.toMap
    writeZip(toZipByteArray(processedFiles), path)
  }

  def gradeLevel(filename: String): Option[String] = {
    def pad(st: String) = st.length match {
      case 1 => s"0$st"
      case _ => st
    }
    val gradeRegex = ".*Gr(ade)?_(\\d*)_.*".r
    filename match {
      case gradeRegex(_, grade) => Some(pad(grade))
      case _ => None
    }
  }

  private def taskInfo(filename: String, subject: String)(implicit metadata: Option[JsValue]): JsObject = {
    val id = metadata.map(md => (md \ "sourceId").as[String])
    partialObj(
      "title" -> id.map(JsString(_)),
      "description" -> id.map(id => JsString(s"${filename.split("/").last}: $id")),
      "gradeLevel" -> gradeLevel(filename).map(Json.arr(_)),
      "subjects" -> Some(Json.obj(
        "primary" -> Some(Json.obj(
          "$oid" -> subject
        ))
      )),
      "domains" -> Some(Json.arr()),
      "extended" -> metadata.map(md => Json.obj(
        "progresstesting" -> md
      ))
    )
  }

  private def postProcess(item: JsValue): JsValue = item match {
    case json: JsObject => {
      val xhtml = BootstrapTranslator.translate(unescapeCss(postprocessHtml((json \ "xhtml").as[String])))
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