package com.keydatasys.conversion.qti

import com.keydatasys.conversion.qti.interactions.{ChoiceInteractionTransformer => KDSChoiceInteractionTransformer, GraphicGapMatchInteractionTransformer => KDSGraphicGapMatchInteractionTransformer, MatchInteractionTransformer => KDSMatchInteractionTransformer, TextEntryInteractionTransformer => KDSTextEntryInteractionTransformer, _}
import com.keydatasys.conversion.qti.processing.ProcessingTransformer
import org.corespring.conversion.qti.interactions._
import org.corespring.conversion.qti.{NodeAndJsonTransformer, QtiTransformer => SuperQtiTransformer}

import scala.xml.Elem
import scala.xml.transform.RewriteRule

object QtiTransformer extends SuperQtiTransformer with ProcessingTransformer {


  override def postXhtmlTransformers: Seq[NodeAndJsonTransformer] = Seq(AudioMarkupAndComponent)

  override def interactionTransformers(qti: Elem) = Seq(
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