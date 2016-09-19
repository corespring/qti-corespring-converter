package org.measuredprogress.conversion.qti

import com.keydatasys.conversion.qti.ItemTransformer
import com.keydatasys.conversion.qti.processing.ProcessingTransformer
import org.corespring.conversion.qti.interactions._
import org.corespring.conversion.qti.{QtiTransformer => SuperQtiTransformer}
import org.measuredprogress.conversion.qti.interactions.{MatchInteractionTransformer => MPMatchInteractionTransformer}

import scala.xml.Elem

object QtiTransformer extends SuperQtiTransformer with ProcessingTransformer {

  override def interactionTransformers(qti: Elem) = Seq(
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
    MPMatchInteractionTransformer,
    NumberedLinesTransformer(qti),
    OrderInteractionTransformer,
    PointInteractionTransformer,
    RubricBlockTransformer,
    SelectTextInteractionTransformer,
    TextEntryInteractionTransformer(qti)
  )

  override def statefulTransformers = Seq(
    FeedbackBlockTransformer,
    NumberedLinesTransformer
  )
}

object ItemTransformer extends ItemTransformer(QtiTransformer)