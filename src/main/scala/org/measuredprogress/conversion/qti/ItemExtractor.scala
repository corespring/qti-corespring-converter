package org.measuredprogress.conversion.qti

import com.keydatasys.conversion.qti.{KDSItemTransformer => KDSItemTransformer}
import org.corespring.common.file.SourceWrapper
import org.corespring.common.json.JsonUtil
import org.corespring.common.util.HtmlProcessor
import org.corespring.conversion.qti.AbstractItemExtractor
import org.corespring.conversion.qti.manifest.{ManifestItem, ManifestReader, QTIManifest}
import play.api.libs.json._

import scala.util.matching.Regex
import scala.xml.{Node, XML}
import scalaz.{Failure, Success, Validation}
import org.corespring.macros.DescribeMacro._
import org.slf4j.LoggerFactory


private[measuredprogress] object MeasuredProgressExtractor extends JsonUtil {

  private lazy val logger = LoggerFactory.getLogger(MeasuredProgressExtractor.this.getClass)

  private val elaId = "4ffb535f6bb41e469c0bf2ac"
  private val mathId = "4ffb535f6bb41e469c0bf2c2"

  private val subjects = Map(
    "Language Arts" -> elaId,
    "Mathematics" -> mathId
  )

  def getId(string: String): String = {
    val itemLogicRegex = "ITEM-LOGIC-(.*).xml".r
    val numberARegex = "(\\d+A*).xml".r
    (string.split("/").last match {
      case itemLogicRegex(id) => Some(id)
      case numberARegex(id) => Some(id)
      case _ => None
    }).getOrElse{
      throw new IllegalArgumentException(s"Cant get id from string: $string")
    }
  }

  private def lomStandardLabel(lom: Node, regex: Regex): Seq[String] = {
    def matches(string: String) = regex.pattern.matcher(string).matches
    (lom \\ "curriculumStandardsMetadataSet" \\ "curriculumStandardsMetadata").filter(metadata => {
      matches((metadata \\ "setOfGUIDs" \ "@region").text)
    }).map(metadata => {
      (metadata \ "setOfGUIDs" \ "labelledGUID" \ "label").text
    })
  }

  private def lomReader(lom: Node, key: String): Option[String] =
    (lom \\ "classification" \\ "taxonPath").seq
      .find(taxon => (taxon \ "source" \ "string").text == key)
      .map(taxon => (taxon \ "taxon" \ "entry" \ "string").text)

  def getProfileJson(manifestItem:ManifestItem, metadata : JsObject) : JsObject = {

    val lom = manifestItem.manifest
    logger.debug(describe(manifestItem))
    partialObj(
      "originId" -> Some(JsString(getId(manifestItem.filename))),
      "taskInfo" -> Some(partialObj(
        "title" -> Some(JsString(getId(manifestItem.filename))),
        "description" -> Some(JsString(manifestItem.id)),
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
//        "gradeLevel" -> lomReader(lom, "GradeLevel")
//          .map(gradeLevel => JsArray(gradeLevel match {
//            case "No" => Seq.empty[JsString]
//            case _ => gradeLevel.split(",").map(grade => grade.length match {
//              case 1 => s"0$grade"
//              case _ => grade
//            }).map(JsString(_))
//          })),
        "extended" -> Some(Json.obj(
          "measuredprogress" -> (metadata ++ Json.obj(
            "sourceId" -> getId(manifestItem.filename),
            "identifier" -> manifestItem.id
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
    )
  }
}

@deprecated("This was used in the old converter, remove asap", "0.28")
private[measuredprogress] class ItemExtractor(sources: Map[String, SourceWrapper], commonMetadata: JsObject, itemTransformer: KDSItemTransformer)
  extends AbstractItemExtractor with HtmlProcessor with JsonUtil {

  private lazy val logger = LoggerFactory.getLogger(this.getClass)

  val elaId = "4ffb535f6bb41e469c0bf2ac"
  val mathId = "4ffb535f6bb41e469c0bf2c2"

  val subjects = Map(
    "Language Arts" -> elaId,
    "Mathematics" -> mathId
  )

  val manifest: Option[QTIManifest] = sources.find{ case(filename, _) => filename == ManifestReader.filename }
    .map { case(_, manifest) => {
      ManifestReader.read(XML.loadString(manifest.getLines.mkString), sources)
    } }

  lazy val ids = manifest.map(manifest => manifest.items.map(_.id)).getOrElse(Seq.empty)

  lazy val measuredProgressId: Map[String, String] = manifest.map(_.items.map(f => {
    f.id -> MeasuredProgressExtractor.getId(f.filename)
  })).getOrElse(Seq.empty).toMap


  lazy val meta: Map[String, Option[JsObject]] =
    manifest.map(_.items.map(f => {
      f.id -> Some(MeasuredProgressExtractor.getProfileJson(f, commonMetadata))
    })).getOrElse(Seq.empty).toMap

  def filesFromManifest(id: String) = manifest.map(m => m.items.find(_.id == id)).flatten.map(item => item.resources)
    .getOrElse(Seq.empty).map(_.path)

  lazy val itemJson: Map[String, Validation[_<:Error, _<:JsValue]] =
    manifest.map(_.items.map(f => sources.get(f.filename).map(s => {
      try {
        f.id -> Success(itemTransformer.transform(preprocessHtml(s.getLines.mkString), f, sources))
      } catch {
        case e: Exception => {
          e.printStackTrace()
          println(s"Error: ${e.getMessage}")
          f.id -> Failure(new Error(s"There was an error translating ${f.id} into CoreSpring JSON"))
        }
      }
    })).flatten).getOrElse(Seq.empty).toMap

  override val metadata: Map[String,  JsValue] = Map.empty
}
