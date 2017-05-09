package com.progresstesting.conversion.qti

import com.keydatasys.conversion.qti.ItemTransformer
import com.keydatasys.conversion.qti.manifest.ManifestReader
import com.keydatasys.conversion.qti.util.{PassageScrubber, PathFlattener, PassageTransformer}
import org.corespring.common.file.SourceWrapper
import org.corespring.common.json.JsonUtil
import org.corespring.common.util.HtmlProcessor
import org.corespring.conversion.qti.AbstractItemExtractor
import org.corespring.conversion.qti.manifest.QTIManifest
import play.api.libs.json._

import scala.xml.Node
import scalaz.{Failure, Success, Validation}


class ItemExtractor(sources: Map[String, SourceWrapper], commonMetadata: JsObject, itemTransformer: ItemTransformer)
  extends AbstractItemExtractor with PassageTransformer with HtmlProcessor with PathFlattener with PassageScrubber with JsonUtil {

  val elaId = "4ffb535f6bb41e469c0bf2ac"
  val scienceId = "4ffb535f6bb41e469c0bf2d1"
  val socialStudiesId = "4ffb535f6bb41e469c0bf2de"
  val mathId = "4ffb535f6bb41e469c0bf2c2"

  val manifest: Option[QTIManifest] = sources.find{ case(filename, _) => filename == ManifestReader.filename }
    .map { case(_, manifest) => {
      ManifestReader.read(manifest, sources)
    } }

  lazy val ids = manifest.map(manifest => manifest.items.map(_.id)).getOrElse(Seq.empty)

  lazy val metadata: Map[String, Validation[Error, Option[JsValue]]] =
    manifest.map(_.items.map(f => {
      f.id -> Success(Some(
        commonMetadata ++ metadataFromManifest(f.manifest, "(.*).xml".r.replaceAllIn(f.filename, "$1"))))
    })).getOrElse(Seq.empty).toMap

  def filesFromManifest(id: String) = manifest.map(m => m.items.find(_.id == id)).flatten.map(item => item.resources)
    .getOrElse(Seq.empty).map(_.path.flattenPath)

  lazy val itemJson: Map[String, Validation[Error, JsValue]] =
    manifest.map(_.items.map(f => sources.get(f.filename.flattenPath).map(s => {
      try {
        f.id -> Success(itemTransformer.transform(scrub(preprocessHtml(s.getLines.mkString)), f, sources.filter{case (filename, _) => !filename.endsWith("css")}))
      } catch {
        case e: Exception => {
          println(s"Err3: ${e.getMessage}")
          f.id -> Failure(new Error(s"There was an error translating ${f.id} into CoreSpring JSON"))
        }
      }
    })).flatten).getOrElse(Seq.empty).toMap


  def metadataFromManifest(node: Node, id: String) = {
    val map = (node \ "metadata" \ "lom" \ "classification" \\ "taxonPath").map(taxon => {
      (taxon \ "source" \ "string").text -> (taxon \ "taxon" \ "entry" \ "string").text
    }).filter(_._2.nonEmpty).toMap
    def getLike(string: String): Option[String] = map.find{ case(key, value) => key.contains(string) }.map(_._2)
    val grades = getLike("grade").map(_.split(",").map(grade => grade.length match {
      case 1 => s"0$grade"
      case _ => grade
    }))
    partialObj(
      "originId" -> Some(JsString(id)),
      "otherAlignments" -> Some(partialObj(
        "depthOfKnowledge" -> getLike("DOK").map(JsString(_)),
        "bloomsTaxonomy" -> getLike("Bloom").map(JsString(_))
      )),
      "taskInfo" -> Some(partialObj(
        "extended" -> Some(Json.obj(
          "progresstesting" -> partialObj(
            "sourceId" -> Some(JsString(id)),
            "teacherOrDistrict" -> getLike("teacherOrDistrict").map(JsString(_))
          )
        )),
        "originId" -> Some(JsString(id)),
        "title" -> Some(JsString(id)),
        "subjects" -> getLike("subject").map(subject => {
          (subject.toLowerCase match {
            case "ela" => Some(elaId)
            case "math" => Some(mathId)
            case "science" => Some(scienceId)
            case "socialstudies" => Some(socialStudiesId)
            case _ => None
          }).map(id => Json.obj("primary" -> Json.obj("$oid" -> id)))
        }).flatten,
        "gradeLevel" -> grades.map(grades => JsArray(grades.map(JsString(_)))),
        "description" -> getLike("teacherOrDistrict").map(JsString(_))
      ))
    )
  }

}
