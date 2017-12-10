package org.corespring.conversion.qti

import com.keydatasys.conversion.qti.processing.ProcessingTransformer
import org.corespring.common.file.SourceWrapper
import org.corespring.common.util.CssSandboxer
import org.corespring.common.xml.XMLNamespaceClearer
import org.corespring.conversion.qti.interactions._
import org.corespring.conversion.qti.manifest.QTIManifest
import org.corespring.conversion.qti.transformers.InteractionRuleTransformer
import org.measuredprogress.conversion.qti.interactions.ImageConverter
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.xml._
import scala.xml.transform._
import org.corespring.macros.DescribeMacro._
import org.corespring.utils.CDataHelper

import scala.xml.parsing.ConstructingParser

object R {

}

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

class InlineCss(sources:Map[String,SourceWrapper]) extends RewriteRule{

  private val logger = LoggerFactory.getLogger(this.getClass)

  def baseName(s: String) : String = {
    val out = s.split("/").last
    out
  }

  override def transform (node: Node) = node match {
    case node: Node if node.label == "stylesheet" => {

      val href = (node \ "@href").text
      val baseHref = baseName (href)

      logger.trace(describe (baseHref))

      /**
        * TODO: This uses base name matching, should have a function that honors the paths in the href
        * relative the path of the qti file.
        */
      val src = sources.collectFirst {
        case (key, src) if (baseName (key) == baseHref) => src
      }

      logger.debug (describe (src) )

      src
        .map (cssSource => <style type="text/css">{CssSandboxer.sandbox (cssSource.getLines.mkString, ".qti.kds")}</style>)
        //.map (cssSource => <style type="text/css">/* {href} */ {CssSandboxer.sandbox (cssSource.getLines.mkString, ".qti.kds")}</style>)
        .getOrElse (throw new IllegalStateException (s"unable to locate stylesheet by name: $name") )

    }
    case _ => node
  }
}

trait QtiTransformer extends XMLNamespaceClearer with ProcessingTransformer with ImageConverter {

  override val logger = LoggerFactory.getLogger(QtiTransformer.this.getClass)

  private val KDSTableReset =
    <style type="text/css">
      {""".kds table,.kds table th{color:initial}.kds table td a,.kds table td a:hover{text-decoration:initial}.kds table tfoot td,.kds table th{background:initial}.kds table{border-collapse:initial;line-height:initial;margin:initial}.kds table td,.kds table th{padding:initial;vertical-align:initial;min-width:initial}.kds table td a{color:inherit}"""}
    </style>

  def interactionTransformers(qti: Elem): Seq[InteractionTransformer]

  def statefulTransformers: Seq[Transformer]

  def transform(qti: Elem): JsValue = {
    val transformers = interactionTransformers(qti)

    /** Need to pre-process Latex so that it is available for all JSON and XML transformations **/
    val texProcessedQti = new RuleTransformer(FontTransformer).transform(new RuleTransformer(TexTransformer).transform(qti))
    val components = transformers.foldLeft(Map.empty[String, JsObject])(
      (map, transformer) => map ++ transformer.interactionJs(texProcessedQti.head, QTIManifest.EmptyManifest))

    val transformedHtml = new RuleTransformer(transformers: _*).transform(texProcessedQti)
    val html = statefulTransformers.foldLeft(clearNamespace((transformedHtml.head \ "itemBody").head))(
      (html, transformer) => transformer.transform(html, QTIManifest.EmptyManifest).head)

    val divRoot = new RuleTransformer(ItemBodyTransformer).transform(html).head

    Json.obj(
      "xhtml" -> divRoot.toString.replaceAll("\\p{Cntrl}", ""),
      "components" -> components) ++ customScoring(qti, components)
  }

  def ItemBodyTransformer = new RewriteRule with XMLNamespaceClearer {

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

    val inlinedCss = new RuleTransformer(new InlineCss(sources)).transform((qti \\ "stylesheet"))

    logger.debug(s"inlined css: $inlinedCss")

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
    /** this is a cheap move - but it means we don't have to get into writing our own jsoup TreeBuilder.
      * The correct way to do this would be getting jsoup to not escape the contents of a <style> node. */
    val inlinedCssString = InlinedCss.apply(qti, sources).mkString("")
    val cssInserted = converted.replaceFirst("<inline_css />", inlinedCssString)
    logger.trace(describe(converted))

    Json.obj(
      "xhtml" -> s"${KDSTableReset} ${cssInserted}",
      "components" -> components.map { case (id, json) => id -> convertJson(json) }) ++ customScoring(qti, components)
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

  def statefulTransformers = Seq(
    FeedbackBlockTransformer,
    NumberedLinesTransformer
  )

}
