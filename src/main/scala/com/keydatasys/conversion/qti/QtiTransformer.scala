package com.keydatasys.conversion.qti

import com.keydatasys.conversion.qti.interactions.{GraphicGapMatchInteractionTransformer => KDSGraphicGapMatchInteractionTransformer, ChoiceInteractionTransformer => KDSChoiceInteractionTransformer, TextEntryInteractionTransformer => KDSTextEntryInteractionTransformer, MatchInteractionTransformer => KDSMatchInteractionTransformer, _}
import com.keydatasys.conversion.qti.processing.ProcessingTransformer
import org.corespring.common.xml.XMLNamespaceClearer
import org.corespring.conversion.qti.interactions._
import org.corespring.conversion.qti.{QtiTransformer => SuperQtiTransformer}
import play.api.libs.json.{Json, JsObject}

import scala.xml.transform.RewriteRule
import scala.xml.{Node, Elem}

object QtiTransformer extends SuperQtiTransformer with ProcessingTransformer {

  override val normalizeScore = false

  override def ItemBodyTransformer = new RewriteRule with XMLNamespaceClearer {

    override def transform(node: Node): Seq[Node] = {
      node match {
        case elem: Elem if elem.label == "itemBody" => {
          <div class="item-body kds qti">{ elem.child }</div>
        }
        case _ => node
      }
    }
  }

  def interactionTransformers(qti: Elem) = Seq(
    AudioComponentTransformer,
    CalculatorWidgetTransformer,
    CorespringTabTransformer,
    CoverflowInteractionTransformer,
    DragAndDropInteractionTransformer,
    ExtendedTextInteractionTransformer,
    FeedbackBlockTransformer(qti),
    FocusTaskInteractionTransformer,
    FoldableInteractionTransformer,
    new HottextInteractionTransformer,
    KDSChoiceInteractionTransformer,
    new KDSGraphicGapMatchInteractionTransformer(),
    LineInteractionTransformer,
    KDSMatchInteractionTransformer,
    NumberedLinesTransformer(qti),
    NumberLineInteractionTransformer,
    new OrderInteractionTransformer,
    PointInteractionTransformer,
    ProtractorWidgetTransformer,
    RubricBlockTransformer,
    RulerWidgetTransformer,
    SelectPointInteractionTransformer(qti),
    SelectTextInteractionTransformer,
    TeacherInstructionsTransformer,
    KDSTextEntryInteractionTransformer(qti)
  )

  def statefulTransformers = Seq(
    FeedbackBlockTransformer,
    NumberedLinesTransformer,
    KDSTextEntryInteractionTransformer
  )

}