package org.measuredprogress.conversion.qti

import com.keydatasys.conversion.qti.ItemTransformer
import org.corespring.common.file.SourceWrapper
import org.corespring.common.json.JsonUtil
import org.corespring.common.util.HtmlProcessor
import org.corespring.conversion.qti.AbstractItemExtractor
import org.corespring.conversion.qti.manifest.{ManifestReader, QTIManifest}
import play.api.libs.json._

import scala.util.matching.Regex
import scala.xml.{Node, XML}
import scalaz.{Failure, Success, Validation}

class ItemExtractor(sources: Map[String, SourceWrapper], commonMetadata: JsObject, itemTransformer: ItemTransformer)
  extends AbstractItemExtractor with HtmlProcessor with JsonUtil {

  val elaId = "4ffb535f6bb41e469c0bf2ac"
  val mathId = "4ffb535f6bb41e469c0bf2c2"

  val subjects = Map(
    "Language Arts" -> elaId,
    "Mathematics" -> mathId
  )

  def lomReader(lom: Node, key: String): Option[String] =
    (lom \\ "classification" \\ "taxonPath").seq
      .find(taxon => (taxon \ "source" \ "string").text == key)
      .map(taxon => (taxon \ "taxon" \ "entry" \ "string").text)

  def lomStandardLabel(lom: Node, regex: Regex): Seq[String] = {
    def matches(string: String) = regex.pattern.matcher(string).matches
    (lom \\ "curriculumStandardsMetadataSet" \\ "curriculumStandardsMetadata").filter(metadata => {
      matches((metadata \\ "setOfGUIDs" \ "@region").text)
    }).map(metadata => {
      (metadata \ "setOfGUIDs" \ "labelledGUID" \ "label").text
    })
  }

  def getId(string: String): String = {
    val itemLogicRegex = "ITEM-LOGIC-(.*).xml".r
    val numberARegex = "(\\d+A*).xml".r
    (string.split("/").last match {
      case itemLogicRegex(id) => Some(id)
      case numberARegex(id) => Some(id)
      case _ => None
    }).getOrElse(throw new Exception(s"Could not parse id for $string"))
  }

  val manifest: Option[QTIManifest] = sources.find{ case(filename, _) => filename == ManifestReader.filename }
    .map { case(_, manifest) => {
      ManifestReader.read(XML.loadString(manifest.getLines.mkString), sources)
    } }

  lazy val ids = manifest.map(manifest => manifest.items.map(_.id)).getOrElse(Seq.empty)

  lazy val measuredProgressId: Map[String, String] = manifest.map(_.items.map(f => {
    f.id -> getId(f.filename)
  })).getOrElse(Seq.empty).toMap

  lazy val meta: Map[String, Option[JsObject]] =
    manifest.map(_.items.map(f => {
      val lom = f.manifest
      println(s""""${f.id}":"${getId(f.filename)}",""")
      f.id -> Some(partialObj(
        "originId" -> Some(JsString(getId(f.filename))),
        "taskInfo" -> Some(commonMetadata ++ partialObj(
          "title" -> Some(JsString(getId(f.filename))),
          "description" -> Some(JsString(f.id)),
          "subjects" ->
            ((lomReader(lom, "Subject")
              .map(subject => subjects.get(subject).map(subject => Json.obj("primary" -> Json.obj("$oid" -> subject)))).flatten) match {
            case Some(subject) => Some(subject)
            case _ => lomStandardLabel(lom, ".*ELA-LITERACY.*".r).nonEmpty match {
              case true => Some(Json.obj("primary" -> Json.obj("$oid" -> elaId)))
              case _ => lomStandardLabel(lom, ".*MATH.*".r).nonEmpty match {
                case true => Some(Json.obj("primary" -> Json.obj("$oid" -> mathId)))
                case _ => None
              }
            }
          }),
          "gradeLevel" -> lomReader(lom, "GradeLevel")
              .map(gradeLevel => JsArray(gradeLevel match {
                case "No" => Seq.empty[JsString]
                case _ => gradeLevel.split(",").map(grade => grade.length match {
                  case 1 => s"0$grade"
                  case _ => grade
                }).map(JsString(_))
              })),
          "extended" -> Some(Json.obj(
            "measuredprogress" -> (commonMetadata ++ Json.obj(
              "sourceId" -> getId(f.filename),
              "identifier" -> f.id
            ))))
        )),
        "otherAlignments" -> ({
          val depthOfKnowledgeRegex = "DOK\\.?.*(\\d+)".r
          val dok: Seq[String] = lomStandardLabel(lom, "DOK.*".r)
          val response: Option[JsValue] = (dok.length match {
            case 1 => dok.head match {
              case depthOfKnowledgeRegex(dok) => Some(Json.obj("depthOfKnowledge" -> dok))
              case _ => None
            }
            case _ => None
          })
          response
        }),
        "standards" -> Some(JsArray(lomStandardLabel(lom, "Common Core State.*".r).map(s => JsString(s.replaceFirst("^.", "").replaceAll(".0", ".")))))
      ))
    })).getOrElse(Seq.empty).toMap

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

  override val metadata: Map[String, Validation[Error, Option[JsValue]]] = Map.empty
}
