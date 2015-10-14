package com.keydatasys.conversion.qti

import com.keydatasys.conversion.qti.interactions.{ ChoiceInteractionTransformer => KDSChoiceInteractionTransformer, TextEntryInteractionTransformer => KDSTextEntryInteractionTransformer, _ }
import org.corespring.conversion.qti.interactions._
import org.corespring.conversion.qti.{QtiTransformer => SuperQtiTransformer}

import scala.xml.Elem

object QtiTransformer extends SuperQtiTransformer {

  def interactionTransformers(qti: Elem) = Seq(
    SelectPointInteractionTransformer(qti),
    KDSChoiceInteractionTransformer,
    TeacherInstructionsTransformer,
    HottextInteractionTransformer,
    RubricBlockTransformer,
    ElementTransformer,
    MatchInteractionTransformer,
    NumberLineInteractionTransformer,
    GraphicGapMatchInteractionTransformer,
    DragAndDropInteractionTransformer,
    FeedbackBlockTransformer(qti),
    NumberedLinesTransformer(qti),
    FocusTaskInteractionTransformer,
    KDSTextEntryInteractionTransformer(qti),
    LineInteractionTransformer,
    OrderInteractionTransformer,
    PointInteractionTransformer,
    SelectTextInteractionTransformer,
    ExtendedTextInteractionTransformer,
    FoldableInteractionTransformer,
    CoverflowInteractionTransformer,
    CorespringTabTransformer,
    CalculatorWidgetTransformer,
    ProtractorWidgetTransformer,
    RulerWidgetTransformer)

  def statefulTransformers = Seq(
    FeedbackBlockTransformer,
    NumberedLinesTransformer,
    KDSTextEntryInteractionTransformer
  )

}