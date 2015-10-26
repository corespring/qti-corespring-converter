package com.keydatasys.conversion.qti

import com.keydatasys.conversion.qti.interactions.{ ChoiceInteractionTransformer => KDSChoiceInteractionTransformer, TextEntryInteractionTransformer => KDSTextEntryInteractionTransformer, MatchInteractionTransformer => KDSMatchInteractionTransformer, _ }
import com.keydatasys.conversion.qti.processing.ProcessingTransformer
import org.corespring.conversion.qti.interactions._
import org.corespring.conversion.qti.{QtiTransformer => SuperQtiTransformer}
import play.api.libs.json.{Json, JsObject}

import scala.xml.{Node, Elem}

object QtiTransformer extends SuperQtiTransformer with ProcessingTransformer {

  override def customScoring(qti: Node, components: Map[String, JsObject]): JsObject = {
    toJs(qti).map(wrap) match {
      case Some(javascript) => Json.obj("customScoring" -> javascript)
      case _ => Json.obj()
    }
  }

  def interactionTransformers(qti: Elem) = Seq(
    SelectPointInteractionTransformer(qti),
    KDSChoiceInteractionTransformer,
    TeacherInstructionsTransformer,
    HottextInteractionTransformer,
    RubricBlockTransformer,
    ElementTransformer,
    KDSMatchInteractionTransformer,
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