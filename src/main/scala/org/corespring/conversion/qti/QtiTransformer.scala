package org.corespring.conversion.qti

import org.corespring.common.file.SourceWrapper
import org.corespring.common.util.CssSandboxer
import org.corespring.common.xml.XMLNamespaceClearer
import org.corespring.conversion.qti.interactions._
import org.corespring.conversion.qti.manifest.QTIManifest
import org.corespring.conversion.qti.processing.ProcessingTransformer
import org.corespring.conversion.qti.transformers.InteractionRuleTransformer
import play.api.libs.json._

import scala.xml._
import scala.xml.transform._

trait QtiTransformer extends XMLNamespaceClearer with ProcessingTransformer {

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
      "components" -> components) //++ customScoring(qti, components)
  }

  def ItemBodyTransformer = new RewriteRule with XMLNamespaceClearer {

    override def transform(node: Node): Seq[Node] = {
      node match {
        case elem: Elem if elem.label == "itemBody" => {
          <div class="item-body qti">{ elem.child }</div>
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

    val finalHtml = QtiTransformer.KDSTableReset.toString ++ new RuleTransformer(new RewriteRule {
      override def transform(node: Node) = node match {
        case node: Node if node.label == "stylesheet" =>
          (sources.find { case (file, source) => file == (node \ "@href").text.split("/").last }.map(_._2)) match {
            case Some(cssSource) =>
              <style type="text/css">{ CssSandboxer.sandbox(cssSource.getLines.mkString, ".qti.kds") }</style>
            case _ => node
          }
        case _ => node
      }
    }, ItemBodyTransformer).transform(html).head.toString

    Json.obj(
      "xhtml" -> finalHtml,
      "components" -> components) ++ customScoring(qti, components)
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
    HottextInteractionTransformer,
    new GraphicGapMatchInteractionTransformer(),
    LineInteractionTransformer,
    MatchInteractionTransformer,
    NumberedLinesTransformer(qti),
    OrderInteractionTransformer,
    PointInteractionTransformer,
    RubricBlockTransformer,
    new SelectTextInteractionTransformer(),
    TextEntryInteractionTransformer(qti)
  )

  def statefulTransformers = Seq(
    FeedbackBlockTransformer,
    NumberedLinesTransformer
  )

  private val KDSTableReset =
    <style type="text/css">{""".kds table,.kds table th{color:initial}.kds table td a,.kds table td a:hover{text-decoration:initial}.kds table tfoot td,.kds table th{background:initial}.kds table{border-collapse:initial;line-height:initial;margin:initial}.kds table td,.kds table th{padding:initial;vertical-align:initial;min-width:initial}.kds table td a{color:inherit}"""}</style>

}
