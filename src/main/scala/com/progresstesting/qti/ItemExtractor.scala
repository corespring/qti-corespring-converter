package com.progresstesting.qti

import com.keydatasys.conversion.qti.ItemTransformer
import com.keydatasys.conversion.qti.manifest.ManifestReader
import org.corespring.common.file.SourceWrapper
import org.corespring.conversion.qti.AbstractItemExtractor
import org.corespring.conversion.qti.manifest.QTIManifest
import play.api.libs.json.{Json, JsValue, JsObject}

import scalaz.{Failure, Success, Validation}

class ItemExtractor(sources: Map[String, SourceWrapper], commonMetadata: JsObject)
  extends AbstractItemExtractor {

  val manifest: Option[QTIManifest] = sources.find{ case(filename, _) => filename == ManifestReader.filename }
    .map { case(_, manifest) => ManifestReader.read(manifest, sources) }

  lazy val ids = manifest.map(manifest => manifest.items.map(_.id)).getOrElse(Seq.empty)

  lazy val metadata: Map[String, Validation[Error, Option[JsValue]]] =
    manifest.map(_.items.map(f =>
      f.id -> Success(Some(commonMetadata ++ Json.obj("sourceId" -> "(.*).xml".r.replaceAllIn(f.filename, "$1"))))
    )).getOrElse(Seq.empty).toMap

  def filesFromManifest(id: String) = manifest.map(m => m.items.find(_.id == id)).flatten.map(item => item.resources)
    .getOrElse(Seq.empty).map(_.path)

  lazy val itemJson: Map[String, Validation[Error, JsValue]] =
    manifest.map(_.items.map(f => sources.get(f.filename).map(s => {
      try {
        f.id -> Success(ItemTransformer.transform(s.getLines.mkString, f, sources))
      } catch {
        case e: Exception => {
          println(e.getMessage)
          f.id -> Failure(new Error(s"There was an error translating ${f.id} into CoreSpring JSON"))
        }
      }
    })).flatten).getOrElse(Seq.empty).toMap

}
