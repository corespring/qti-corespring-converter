package com.keydatasys.conversion.qti

import com.keydatasys.conversion.qti.interactions.{ChoiceInteractionTransformer => KDSChoiceInteractionTransformer, GraphicGapMatchInteractionTransformer => KDSGraphicGapMatchInteractionTransformer, MatchInteractionTransformer => KDSMatchInteractionTransformer, TextEntryInteractionTransformer => KDSTextEntryInteractionTransformer, _}
import com.keydatasys.conversion.qti.processing.ProcessingTransformer
import org.corespring.conversion.qti.interactions._
import org.corespring.conversion.qti.{QtiTransformer => SuperQtiTransformer}

import scala.xml.Elem

object QtiTransformer extends SuperQtiTransformer with ProcessingTransformer {

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