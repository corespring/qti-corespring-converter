package org.corespring.conversion.qti

import com.keydatasys.conversion.qti.processing.ProcessingTransformer
import org.corespring.common.file.SourceWrapper
import org.corespring.common.html.JsoupParser
import org.corespring.common.util.CssSandboxer
import org.corespring.common.xml.XMLNamespaceClearer
import org.corespring.conversion.qti.interactions._
import org.corespring.conversion.qti.transformers.InteractionRuleTransformer
import org.corespring.macros.DescribeMacro._
import scala.collection.JavaConversions._
import org.corespring.utils.CDataHelper
import org.measuredprogress.conversion.qti.interactions.ImageConverter
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.xml._
import scala.xml.transform._


object InlinedCss {

  val logger = LoggerFactory.getLogger(InlinedCss.this.getClass)

  def baseName(s: String) : String = {
    val out = s.split("/").last
    out
  }

  def apply(node:Node, sources: Map[String, SourceWrapper]) : Seq[String] = {

    (node \\ "stylesheet").map{ n =>

      val href = (n \ "@href").text
      val baseHref = baseName(href)

      logger.trace(describe(baseHref))

      /**
        * TODO: This uses base name matching, should have a function that honors the paths in the href
        * relative the path of the qti file.
        */
      val src = sources.collectFirst {
        case (key, src) if (baseName (key) == baseHref) => src
      }

      logger.debug (describe (src) )

      src
        .map (cssSource => {
          s"""<style type="text/css">
            ${CssSandboxer.sandbox(cssSource.getLines.mkString, ".qti.kds")}
          </style>"""
        })
        //.map (cssSource => <style type="text/css">/* {href} */ {CssSandboxer.sandbox (cssSource.getLines.mkString, ".qti.kds")}</style>)
        .getOrElse (throw new IllegalStateException (s"unable to locate stylesheet by name: $href") )

    }
  }
}

trait NodeAndJsonTransformer {
  def transform(n:Elem, json:Map[String,JsObject]) : (Elem, Map[String,JsObject])
}

trait QtiTransformer extends XMLNamespaceClearer with ProcessingTransformer with ImageConverter {

  override val logger = LoggerFactory.getLogger(QtiTransformer.this.getClass)

  private val KDSTableReset =
    <style type="text/css">
      {""".kds table,.kds table th{color:initial}.kds table td a,.kds table td a:hover{text-decoration:initial}.kds table tfoot td,.kds table th{background:initial}.kds table{border-collapse:initial;line-height:initial;margin:initial}.kds table td,.kds table th{padding:initial;vertical-align:initial;min-width:initial}.kds table td a{color:inherit}"""}
    </style>

  def interactionTransformers(qti: Elem): Seq[InteractionTransformer]

  def postXhtmlTransformers : Seq[NodeAndJsonTransformer] = Seq.empty

  def statefulTransformers: Seq[Transformer]

  final val ItemBodyTransformer = new RewriteRule {

    override def transform(node: Node): Seq[Node] = {
      node match {
        case elem: Elem if elem.label == "itemBody" => {
          <div class="item-body qti">
            {elem.child}
          </div>
        }
        case _ => node
      }
    }
  }


  def customScoring(qti: Node, components: Map[String, JsObject]): JsObject = {
    toJs(qti).map(wrap) match {
      case Some(javascript) => Json.obj("customScoring" -> javascript)
      case _ => Json.obj()
    }
  }

  private def convertHtml(html: String): String = {
    /** Note: we parse as xml so as not to corrupt the markup structure */
    val doc = JsoupParser.parse(html)
    doc.select("object").foreach(obj => {
      obj.attr("type") match {
        case "image/png" => {
          val img = doc.createElement("img")
          img.attr("src", obj.attr("data").split("/").last)
          obj.replaceWith(img)
        }
        case _ => {}
      }
    })
    doc.outerHtml()
  }

  def transform(qti: Elem, sources: Map[String, SourceWrapper], manifest: Node): JsValue = {
    val transformers = interactionTransformers(qti)

    /** Need to pre-process Latex so that it is available for all JSON and XML transformations **/
    val texProcessedQti = new InteractionRuleTransformer(FontTransformer)
      .transform(new InteractionRuleTransformer(TexTransformer).transform(qti, manifest), manifest)

    val components = transformers.foldLeft(Map.empty[String, JsObject])(
      (map, transformer) => map ++ transformer.interactionJs(texProcessedQti.head, manifest))

    val transformedHtml = new InteractionRuleTransformer(transformers: _*).transform(texProcessedQti, manifest)
    val html = statefulTransformers.foldLeft(clearNamespace((transformedHtml.head \ "itemBody").head))(
      (html, transformer) => transformer.transform(html, manifest).head)

    val addInlinedCssToBody = new RewriteRule{
      override def transform(n:Node) = {
        n match {
          case n: Elem if (n.label == "itemBody") => n.copy(child = (<inline_css/> ++ n.child))
          case _ => n
        }
      }
    }

    val finalHtml  = new RuleTransformer(
       addInlinedCssToBody,
      ItemBodyTransformer
    ).transform(html).head.toString

    logger.trace(describe(finalHtml))
    val noCdata = CDataHelper.stripCDataTags(finalHtml)
    val converted = convertHtml(noCdata)

    /**
      * Some transformers work best once we have the xhtml prepared.
      * The restriction being that the only work with xhtml not qti.
      * Apply those transformations last.
      */
    val xhtmlNode : Elem = XML.loadString(converted)
    val (markup, comps) = postXhtmlTransformers.foldLeft((xhtmlNode -> components))((t, transformer) => {
      val (xhtml, components) = t
      transformer.transform(xhtml, components)
    })

    /** this is a cheap move - but it means we don't have to get into writing our own jsoup TreeBuilder.
      * The correct way to do this would be getting jsoup to not escape the contents of a <style> node. */
    val inlinedCssString = InlinedCss.apply(qti, sources).mkString("")
    val cssInserted = markup.toString.replaceFirst("<inline_css/>", inlinedCssString)
    logger.trace(describe(converted))

    // remove convertJson - escapes values ..."components" -> components.map { case (id, json) => id -> convertJson(json) }) ++ customScoring(qti, components)
    Json.obj(
      "xhtml" -> s"${KDSTableReset} ${cssInserted}",
      "components" -> comps.map { case (id, json) => id -> json }) ++ customScoring(qti, components)
  }

}

object QtiTransformer extends QtiTransformer {

  def interactionTransformers(qti: Elem) = Seq(
    CalculatorTransformer,
    ChoiceInteractionTransformer,
    CorespringTabTransformer,
    CoverflowInteractionTransformer,
    DragAndDropInteractionTransformer,
    ExtendedTextInteractionTransformer,
    FeedbackBlockTransformer(qti),
    FocusTaskInteractionTransformer,
    FoldableInteractionTransformer,
    new HottextInteractionTransformer,
    new GraphicGapMatchInteractionTransformer(),
    LineInteractionTransformer,
    MatchInteractionTransformer,
    NumberedLinesTransformer(qti),
    new OrderInteractionTransformer,
    PointInteractionTransformer,
    RubricBlockTransformer,
    SelectTextInteractionTransformer,
    TextEntryInteractionTransformer(qti)
  )

  val statefulTransformers = Seq(
    FeedbackBlockTransformer,
    NumberedLinesTransformer
  )

}
