package org.corespring.conversion.qti

import org.specs2.mutable.Specification
import play.api.libs.json.Json


class BadMarkupSpec extends Specification with BaseRunnerUtils {
  /**
    * Checks that the markup structure isn't altered
    * (This was happening when we were using the JSoup html parser)
    * @see https://github.com/jhy/jsoup/issues/436
    */
  "kds" should {

    val content =
      """
        |<selectPointInteraction
        |  responseIdentifier="RESPONSE1"
        |  minChoices="0"
        |  maxChoices="0"
        |  class="singleLineSelectPointInteraction">
        |  <prompt visible="true">
        |  <![CDATA[<strong><b class="frac">bold</b></strong>]]>
        |  </prompt>
        |  <object></object></selectPointInteraction>
        |  """.stripMargin.trim

    val rd =  <responseDeclaration identifier="RESPONSE1" cardinality="record">
      <fieldValue identifier="startPointXCoordinate" baseType="float"/>
      <fieldValue identifier="startPointYCoordinate" baseType="float"/>
      <fieldValue identifier="endPointXCoordinate" baseType="float"/>
      <fieldValue identifier="endPointYCoordinate" baseType="float"/>
      <fieldValue identifier="XIntercept" baseType="float"/>
      <fieldValue identifier="YIntercept" baseType="float"/>
      <fieldValue identifier="Slope" baseType="float"/>
      <correctResponse>
        <value startPointXCoordinate="-5" startPointYCoordinate="2" startPointConsider="0" startPointTolerance="0"
               endPointXCoordinate="5" endPointYCoordinate="2" endPointConsider="0" endPointTolerance="0"
               xIntercept="" xInterceptConsider="0" xInterceptTolerance="0" yIntercept="2" yInterceptConsider="1"
               yInterceptTolerance="0.25" slope="0" slopeConsider="1" slopeTolerance="0.25"/>
      </correctResponse>
    </responseDeclaration>


    val item = new ItemBuilder()
      .addToItemBody(content)
      .addResponseDeclaration(rd).xml()

    val zip = new Builder().addItem("1", item).build()

    val out = zip.getParent.resolve("out.zip")

    RunHelper.run(
      zip.toAbsolutePath.toString,
      out.toAbsolutePath.toString,
      "kds",
      None,
      """{"scoringType" : "SBAC"}"""
    )

    val playerDefJson = loadFirstPlayerDefJson(out).getOrElse(Json.obj())

    "it works" in {
      val xhtml = (playerDefJson \ "xhtml").as[String]
      xhtml.indexOf("<strong><b class=\"frac\">bold</b></strong>") > -1 must_== true
    }
  }
}
