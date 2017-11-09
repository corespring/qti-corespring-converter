package org.corespring.conversion.qti

import com.keydatasys.conversion.qti.ItemTransformer
import org.corespring.common.file.SourceWrapper
import org.corespring.common.util.HtmlProcessor
import org.corespring.conversion.qti.manifest.{ManifestReader, QTIManifest}
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.xml.XML
import scalaz.{Failure, Success, Validation}

@deprecated("Old item extractor - very slow", "0.31")
class ItemExtractor(sources: Map[String, SourceWrapper], commonMetadata: JsObject, itemTransformer: ItemTransformer)
  extends AbstractItemExtractor with HtmlProcessor {

  val manifest: Option[QTIManifest] = sources.find{ case(filename, _) => filename == ManifestReader.filename }
    .map { case(_, manifest) => {
      ManifestReader.read(XML.loadString(manifest.getLines.mkString), sources)
    } }

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
        f.id -> Success(itemTransformer.transform(preprocessHtml(s.getLines.mkString), f, sources))
      } catch {
        case e: Exception => {
          println(s"Error: ${e.getMessage}")
          f.id -> Failure(new Error(s"There was an error translating ${f.id} into CoreSpring JSON"))
        }
      }
    })).flatten).getOrElse(Seq.empty).toMap

}
