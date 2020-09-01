package com.keydatasys.conversion.qti

import java.util.zip.ZipFile

import com.keydatasys.conversion.qti.manifest.ManifestReader
import com.keydatasys.conversion.qti.util.{PassageScrubber, PassageTransformer, PathFlattener}
import org.corespring.common.file.SourceWrapper
import org.corespring.common.json.JsonUtil
import org.corespring.common.util.HtmlProcessor
import org.corespring.conversion.qti.AbstractItemExtractor
import org.corespring.conversion.qti.manifest.QTIManifest
import play.api.libs.json._

import scala.xml.Node
import scalaz.{Failure, Success, Validation}

object MetadataExtractor {

  def sourceIdObj(id: String) = {
    Json.obj("sourceId" -> id)
  }

}

class ItemExtractor(zip: ZipFile, sources: Map[String, SourceWrapper], commonMetadata: JsObject, itemTransformer: KDSItemTransformer)
  extends AbstractItemExtractor with PassageTransformer with HtmlProcessor with PathFlattener with PassageScrubber with JsonUtil {

  val manifest: Option[QTIManifest] = sources.find{ case(filename, _) => filename == ManifestReader.filename }
    .map { case(_, manifest) => {
      ManifestReader.read(manifest, sources)
    } }

  lazy val ids = manifest.map(manifest => manifest.items.map(_.id)).getOrElse(Seq.empty)

  lazy val metadata: scala.collection.immutable.Map[String, JsValue] = {

    val oo : Map[String, JsValue] = manifest match {
      case Some(m) => {
        val s : Seq[(String, JsObject)] = m.items.map(f => {
          f.id -> (commonMetadata ++ Json.obj("sourceId" -> "(.*).xml".r.replaceAllIn(f.filename, "$1")))
        })
        s.toMap
//        val sss : Map[String, JsObject] = s.toMap


//        val o = sss.map( t => {
//          t._1 -> Success(Some(t._2))
//        })
//        o
      }
      case None => Map.empty
    }
    //    manifest.map(_.items.map(f => {
    //      f.id -> Success(Some(commonMetadata ++ Json.obj("sourceId" -> "(.*).xml".r.replaceAllIn(f.filename, "$1"))))
    //    })).getOrElse(Seq.empty).toMap

//    val out : Map[String, Validation[Error,Option[JsValue]]] = Map("foo"-> Success(Some(JsString("hi"))))
//    out
    oo
    //    manifest.map(_.items.map(f => {
    //      f.id -> Success(Some(commonMetadata ++ Json.obj("sourceId" -> "(.*).xml".r.replaceAllIn(f.filename, "$1"))))
    //    })).getOrElse(Seq.empty).toMap

  }
  //Map("foo"-> Success(Some(JsString("hi"))))
//    manifest.map(_.items.map(f => {
//      f.id -> Success(Some(commonMetadata ++ Json.obj("sourceId" -> "(.*).xml".r.replaceAllIn(f.filename, "$1"))))
//    })).getOrElse(Seq.empty).toMap

  def filesFromManifest(id: String) = manifest.map(m => m.items.find(_.id == id)).flatten.map(item => item.resources)
    .getOrElse(Seq.empty).map(_.path.flattenPath)

  lazy val itemJson: Map[String, Validation[_ <: Error, _ <: JsValue]] =
    manifest.map(_.items.map(f => sources.get(f.filename.flattenPath).map(s => {
      try {
        f.id -> Success(itemTransformer.transform(scrub(preprocessHtml(s.getLines.mkString)), f, sources))
      } catch {
        case e: Exception => {
          println(s"Err3: ${e.getMessage}")
          f.id -> Failure(new Error(s"There was an error translating ${f.id} into CoreSpring JSON"))
        }
      }
    })).flatten).getOrElse(Seq.empty).toMap

}
