package org.measuredprogress.conversion.qti

import com.keydatasys.conversion.qti.{ItemTransformer => KDSItemTransformer}
import com.keydatasys.conversion.qti.processing.ProcessingTransformer
import org.corespring.conversion.qti.interactions._
import org.corespring.conversion.qti.{QtiTransformer => SuperQtiTransformer}
import org.measuredprogress.conversion.qti.interactions.{TextEntryInteractionTransformer => MPTextEntryInteractionTransformer, HottextInteractionTransformer => MPHottextInteractionTransformer, GraphicGapMatchInteractionTransformer => MPGraphicGapMatchInteractionTransformer, ExtendedTextInteractionTransformer => MPExtendedTextInteractionTransformer, MatchInteractionTransformer => MPMatchInteractionTransformer, RubricBlockTransformer => MPRubricBlockTransformer, DefaultFeedbackTransformer, GapMatchInteractionTransformer}

import scala.xml.Elem

object QtiTransformer extends SuperQtiTransformer with ProcessingTransformer {

  override def interactionTransformers(qti: Elem) = {
    val hasFeedback: Seq[InteractionTransformer] = Seq(
      ChoiceInteractionTransformer,
      new MPHottextInteractionTransformer(),
      MPGraphicGapMatchInteractionTransformer,
      MPMatchInteractionTransformer,
      OrderInteractionTransformer,
      SelectTextInteractionTransformer
    )

    Seq(
      CalculatorTransformer,
      CorespringTabTransformer,
      CoverflowInteractionTransformer,
      DragAndDropInteractionTransformer,
      MPExtendedTextInteractionTransformer,
      FeedbackBlockTransformer(qti),
      FocusTaskInteractionTransformer,
      FoldableInteractionTransformer,
      LineInteractionTransformer,
      NumberedLinesTransformer(qti),
      PointInteractionTransformer,
      MPRubricBlockTransformer,
      new MPTextEntryInteractionTransformer(qti),
      GapMatchInteractionTransformer,
      new UnsupportedInteractionTransformer("drawingInteraction"),
      new UnsupportedInteractionTransformer("hotspotInteraction")
    ) ++ hasFeedback.map(DefaultFeedbackTransformer(_))
  }

  override def statefulTransformers = Seq(
    FeedbackBlockTransformer,
    NumberedLinesTransformer
  )
}

object ItemTransformer extends KDSItemTransformer(QtiTransformer)