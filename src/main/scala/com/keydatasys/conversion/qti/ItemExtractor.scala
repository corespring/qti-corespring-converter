package com.keydatasys.conversion.qti

import com.keydatasys.conversion.qti.manifest.ManifestReader
import com.keydatasys.conversion.qti.util.{PathFlattener, HtmlProcessor, PassageTransformer}
import org.corespring.common.file.SourceWrapper
import org.corespring.conversion.qti.manifest.QTIManifest
import play.api.libs.json._

import scalaz.{Failure, Success, Validation}

class ItemExtractor(sources: Map[String, SourceWrapper], commonMetadata: JsObject)
  extends AbstractItemExtractor with PassageTransformer with HtmlProcessor {

  import PathFlattener._

  val manifest: Option[QTIManifest] = sources.find{ case(filename, _) => filename == ManifestReader.filename }
    .map { case(_, manifest) => ManifestReader.read(manifest, sources) }

  lazy val ids = manifest.map(manifest => manifest.items.map(_.id)).getOrElse(Seq.empty)

  lazy val metadata: Map[String, Validation[Error, Option[JsValue]]] =
    manifest.map(_.items.map(f =>
      f.id -> Success(Some(Json.obj(
        "taskInfo" -> Json.obj("extended" -> Json.obj("kds" -> (Json.obj(
          "sourceId" -> "(.*).xml".r.replaceAllIn(f.filename, "$1")) ++ commonMetadata))))))
    )).getOrElse(Seq.empty).toMap

  def filesFromManifest(id: String) = manifest.map(m => m.items.find(_.id == id)).flatten.map(item => item.resources)
    .getOrElse(Seq.empty).map(_.path.flattenPath)

  lazy val itemJson: Map[String, Validation[Error, JsValue]] =
    manifest.map(_.items.map(f => sources.get(f.filename.flattenPath).map(s => {
      try {
        f.id -> Success(ItemTransformer.transform(preprocessHtml(s.getLines.mkString), f, sources))
      } catch {
        case e: Exception => {
          e.printStackTrace()
          f.id -> Failure(new Error(s"There was an error translating ${f.id} into CoreSpring JSON"))
        }
      }
    })).flatten).getOrElse(Seq.empty).toMap

}
