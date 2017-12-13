package org.corespring.conversion.qti.interactions

import org.corespring.common.xml.XhtmlParser
import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json.{JsArray, JsObject, Json}

class MatchInteractionTransformerSpec extends Specification {

  val responses = Map(
    ("SA", "Antelope") -> ("RB", "Antelope"),
    ("SB", "Beaver") -> ("RA" -> "Beaver"),
    ("SC", "Chinchilla") -> ("RD" -> "Chinchilla"),
    ("SD", "Donkey") -> ("RC" -> "Donkey")
  )

  val cornerText = "Corner"

  val identifier = "RESPONSE"

  val qti = mkQti(addCdata=true)


  def mkQti(addCdata:Boolean) = {


    def labelString(s:String) = if(addCdata) s"<![CDATA[$s]]>" else s

    val s =
      s"""<assessmentItem xmlns="http://www.imsglobal.org/xsd/imsqti_v2p1"
                            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                            xmlns:m="http://www.w3.org/1998/Math/MathML"
                            xsi:schemaLocation="http://www.imsglobal.org/xsd/imsqti_v2p1  http://www.imsglobal.org/xsd/qti/qtiv2p1/imsqti_v2p1p1.xsd"
                            identifier="item-128727" title="Sample Export - Reading Advanced - 128727" adaptive="false" timeDependent="false">
    <responseDeclaration identifier="${identifier}" cardinality="multiple" baseType="directedPair">
      <correctResponse>
        ${
          responses.map{ case ((columnId, columnLabel), (rowId, rowLabel)) => {
            s"""<value>${columnId} ${rowId}</value>"""
          }}.mkString("")
        }
      </correctResponse>
      <mapping defaultValue="0">
        ${
          responses.map { case ((columnId, columnLabel), (rowId, rowLabel)) => {
            s"""<mapEntry mapKey="$columnId $rowId" mappedValue="1"/>"""
          }}.mkString("")
        }
      </mapping>
    </responseDeclaration>
    <outcomeDeclaration baseType="float" cardinality="single" identifier="SCORE"/>
    <stylesheet href="styles/export_package_reading_advanced.css" type="text/css"/>
    <itemBody>
      <div>
        <object type="text/html" data="passages/5578.html" />
      </div>
      <div>Matching Interaction for Passage 1</div>
      <matchInteraction responseIdentifier="$identifier" shuffle="false" maxAssociations="4">
        <prompt>$cornerText</prompt>
        <simpleMatchSet>
          ${
            responses.keys.toSeq.sortBy(_._1).map { case(id, label) => {
              s"""<simpleAssociableChoice identifier="${id}" matchMax="1" fixed="true">
                ${labelString(label)}
              </simpleAssociableChoice>"""
            }}.mkString("")
          }
        </simpleMatchSet>
        <simpleMatchSet>
          ${
            responses.values.toSeq.sortBy(_._1).map { case(id, label) => {
              s"""<simpleAssociableChoice identifier="${id}" matchMax="1" fixed="true">
                ${labelString(label)}
              </simpleAssociableChoice>"""
            }}.mkString("")
          }
        </simpleMatchSet>
      </matchInteraction>
    </itemBody>
  </assessmentItem>"""
  println(s)
    XhtmlParser.loadString(s)
  }

  "interactionJs" should {

    val json = MatchInteractionTransformer.interactionJs(qti, QTIManifest.EmptyManifest).head._2
    "have componentType = 'corespring-match'" in {
      (json \ "componentType").as[String] must be equalTo("corespring-match")
    }

    "correctResponse" should {

      "contain an array of true/false values corresponding to the index of the correct value" in {
        val idsFromJson = (json \ "correctResponse").as[Seq[JsObject]].map { response =>
          (response \ "id").as[String] ->
            responses.values.toSeq.sortBy(_._1).toSeq((response \ "matchSet").as[Seq[Boolean]].indexOf(true))._1
        }.toMap
        val idsFromSource =
          responses.map { case ((columnId, columnLabel), (rowId, rowLabel)) => { columnId -> rowId } }.toMap
        idsFromJson must be equalTo(idsFromSource)
      }

    }

    "allowPartialScoring should be false" in {
      (json \ "allowPartialScoring").as[Boolean] must beFalse
    }


    "feedback" should {

      "have correctFeedbackType set to 'none'" in {
        (json \ "feedback" \ "correctFeedbackType").as[String] must be equalTo("none")
      }

      "have partialFeedbackType set to 'none'" in {
        (json \ "feedback" \ "partialFeedbackType").as[String] must be equalTo("none")
      }

      "have incorrectFeedbackType set to 'none'" in {
        (json \ "feedback" \ "incorrectFeedbackType").as[String] must be equalTo("none")
      }

    }

    "partialScoring" should {

      "be set to an array containing an empty object" in {
        (json \ "partialScoring").as[JsArray] must be equalTo(Json.arr(Json.obj()))
      }

    }

    "model" should {

      val model = (json \ "model").as[JsObject]

      "columns" should {

        val columns = (model \ "columns").as[Seq[JsObject]]

        "start with corner text" in {
          (columns.head \ "labelHtml").as[String] must be equalTo(cornerText)
        }

        "followed by response labels" in {
          (columns.tail).map(column => (column \ "labelHtml").as[String]) must be equalTo(responses.values.toSeq.sortBy(_._1).map(_._2).seq)
        }

      }

      "rows" should {

        val rows = (model \ "rows").as[Seq[JsObject]]

        "contain rows with ID and label" in {
          rows.map(row => ((row \ "id").as[String], (row \ "labelHtml").as[String])) must be equalTo(responses.keys.toSeq)
        }

      }

      "config" should {

        val config = (model \ "config").as[JsObject]

        "inputType should be set to 'radiobutton'" in {
          (config \ "inputType").as[String] must be equalTo("radiobutton")
        }

        "shuffle should be set to false" in {
          (config \ "shuffle").as[Boolean] must be equalTo(false)
        }

      }

    }

  }

}
