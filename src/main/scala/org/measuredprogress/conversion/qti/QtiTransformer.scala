package org.measuredprogress.conversion.qti

import com.keydatasys.conversion.qti.ItemTransformer
import com.keydatasys.conversion.qti.processing.ProcessingTransformer
import org.corespring.conversion.qti.interactions._
import org.corespring.conversion.qti.{QtiTransformer => SuperQtiTransformer}
import org.measuredprogress.conversion.qti.interactions.{GapMatchInteractionTransformer, MatchInteractionTransformer => MPMatchInteractionTransformer, RubricBlockTransformer => MPRubricBlockTransformer, HottextInteractionTransformer => MPHottextInteractionTransformer}

import scala.xml.Elem

object QtiTransformer extends SuperQtiTransformer with ProcessingTransformer {

  override def interactionTransformers(qti: Elem) = {
    println(qti.toString)
    Seq(
      CalculatorTransformer,
      ChoiceInteractionTransformer,
      CorespringTabTransformer,
      CoverflowInteractionTransformer,
      DragAndDropInteractionTransformer,
      ExtendedTextInteractionTransformer,
      FeedbackBlockTransformer(qti),
      FocusTaskInteractionTransformer,
      FoldableInteractionTransformer,
      MPHottextInteractionTransformer,
      new GraphicGapMatchInteractionTransformer(),
      LineInteractionTransformer,
      MPMatchInteractionTransformer,
      NumberedLinesTransformer(qti),
      OrderInteractionTransformer,
      PointInteractionTransformer,
      MPRubricBlockTransformer,
      SelectTextInteractionTransformer,
      TextEntryInteractionTransformer(qti),
      GapMatchInteractionTransformer
    )
  }

  override def statefulTransformers = Seq(
    FeedbackBlockTransformer,
    NumberedLinesTransformer
  )
}

object ItemTransformer extends ItemTransformer(QtiTransformer)