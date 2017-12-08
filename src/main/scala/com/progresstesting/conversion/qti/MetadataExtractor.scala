package com.progresstesting.conversion.qti

import org.corespring.common.json.JsonUtil
import play.api.libs.json._

import scala.xml.Node


object MetadataExtractor extends JsonUtil{

  val elaId = "4ffb535f6bb41e469c0bf2ac"
  val scienceId = "4ffb535f6bb41e469c0bf2d1"
  val socialStudiesId = "4ffb535f6bb41e469c0bf2de"
  val mathId = "4ffb535f6bb41e469c0bf2c2"
  /**
    * extract metadata from <resource></resource>
    * @param node
    * @param id
    * @return
    */
  def metadataFromResourceNode(node: Node, id: String) = {
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

