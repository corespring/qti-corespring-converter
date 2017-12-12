package org.corespring.conversion.qti

import org.specs2.mutable.Specification
import play.api.libs.json.Json
import org.corespring.macros.DescribeMacro.{describe => d}
import org.slf4j.LoggerFactory

class CssIsInlinedSpec extends Specification with BaseRunnerUtils {

  private val logger = LoggerFactory.getLogger(this.getClass)


  val css =
    """table.frac {
      |	display: inline-block;
      |	*display: inline;
      |	vertical-align: middle;
      |	font-size: 85%;
      |	border: 0px;
      |	margin: 0px;
      |	padding: 0px;
      |}
      |
      |.frac .nu {
      |	border-left: 0px;
      |	border-top: 0px;
      |	border-right: 0px;
      |	border-bottom: #000000 1px solid;
      |	text-align: center;
      |}
      |
      |.frac .de {
      |	border: 0px;
      |	text-align: center;
      |}
      |""".stripMargin

  "runner" should {

    "inline css if declared in the qti" in {

      val item = new ItemBuilder()
        .addStylesheet(<stylesheet href="style/LiveInspect.css" type="text/css"/>)
        .addToItemBody(
          """
                  <teacherInstructions>
            <![CDATA[
            <strong>TEACHER READS:</strong><br /><br />
            Read and complete the task that follows.]]>
            </teacherInstructions>""".trim)
        .xml()

      val zip = new Builder()
        .addItem("1", item)
        .addCss(css, "style/LiveInspect.css")
        .build()

      val out = zip.getParent.resolve("out.zip")

      RunHelper.run(
        zip.toAbsolutePath.toString,
        out.toAbsolutePath.toString,
        "kds",
        None,
        """{"scoringType" : "SBAC"}"""
      )

      val playerDefJson = loadFirstPlayerDefJson(out).getOrElse(Json.obj())

      logger.info(d(playerDefJson))

      (playerDefJson \ "xhtml").as[String].contains(".qti.kds .frac .nu") must_== true
//      playerDefJson must_== Json.obj()
    }
  }
}
