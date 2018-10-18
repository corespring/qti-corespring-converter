package org.corespring.conversion.qti

import com.keydatasys.conversion.qti.processing.{ProcessingTransformer, V2JavascriptWrapper}
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

trait QtiTransformer extends XMLNamespaceClearer with ProcessingTransformer with ImageConverter {


  /**
    * Some js custom processing wants the score to be normalized to between 0 - 1.
    * Some providers always do this though, so we need to turn it on/off depending on the provider and the item.
    * At the moment MeasuredProgress are the only ones that need it normalized.
    * @return
    */
  def normalizeDenominator(resource:Node, qti: Node) : Option[Int]

  private lazy val logger = LoggerFactory.getLogger(QtiTransformer.this.getClass)

  private val KDSTableReset =
    <style type="text/css">
      {""".kds table,.kds table th{color:initial}.kds table td a,.kds table td a:hover{text-decoration:initial}.kds table tfoot td,.kds table th{background:initial}.kds table{border-collapse:initial;line-height:initial;margin:initial}.kds table td,.kds table th{padding:initial;vertical-align:initial;min-width:initial}.kds table td a{color:inherit}"""}
    </style>

  def interactionTransformers(qti: Elem): Seq[InteractionTransformer]

  def statefulTransformers: Seq[Transformer]


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


  private def customScoring(
                             qti: Node,
                             components: Map[String, JsObject],
                             denominator: Option[Int]): JsObject = {

    logger.info(describe(denominator))

    toJs(qti).map(V2JavascriptWrapper.wrap(_, denominator)) match {
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



    def baseName(s: String) : String = {
      val out = s.split("/").last
      logger.trace(describe(out, s))
      out
    }

    val finalHtml = new RuleTransformer(new RewriteRule {
      override def transform(node: Node) = node match {
        case node: Node if node.label == "stylesheet" => {

          logger.trace(s"sources - names: ${sources.map(_._1)}")
          val href = (node \ "@href").text
          val baseHref = baseName(href)

          logger.trace(describe(baseHref))

          /**
            * TODO: This uses base name matching, should have a function that honors the paths in the href
            * relative the path of the qti file.
            */
          val src = sources.collectFirst {
            case (key, src) if (baseName(key) == baseHref) => src
          }

          logger.debug(describe(src))

          src
            .map(cssSource => <style type="text/css">
              {CssSandboxer.sandbox(cssSource.getLines.mkString, ".qti.kds")}
            </style>)
            .getOrElse(throw new IllegalStateException(s"unable to locate stylesheet by name: $name"))

        }
        case _ => node
      }
    }, ItemBodyTransformer).transform(html).head.toString

    val id = (qti \ "@identifier").text.trim
    logger.trace(describe(id))
    val converted = convertHtml(finalHtml)
    val maybeResource = (manifest \ "resources" \\ "resource").find( n => n.attribute("identifier").map(_.mkString("")) == Some(id))

    if(maybeResource.isEmpty){
      throw new Exception(s"Cant find resource for id: $id")
    }

    val denominator = normalizeDenominator(maybeResource.get, qti)

    Json.obj(
      "xhtml" -> s"${KDSTableReset} ${converted}",
      "components" -> components.map { case (id, json) => id -> convertJson(json) }) ++
      customScoring(qti, components, denominator)
  }

}

object QtiTransformer extends QtiTransformer {

  override def normalizeDenominator(resource:Node, qti: Node) = {
    toJs(qti).map(t => t.responseVars.length)
  }


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
