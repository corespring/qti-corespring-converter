package com.keydatasys.conversion.qti

import com.keydatasys.conversion.qti.interactions.{GraphicGapMatchInteractionTransformer => KDSGraphicGapMatchInteractionTransformer, ChoiceInteractionTransformer => KDSChoiceInteractionTransformer, TextEntryInteractionTransformer => KDSTextEntryInteractionTransformer, MatchInteractionTransformer => KDSMatchInteractionTransformer, _}
import com.keydatasys.conversion.qti.processing.ProcessingTransformer
import org.corespring.common.xml.XMLNamespaceClearer
import org.corespring.conversion.qti.interactions._
import org.corespring.conversion.qti.{QtiTransformer => SuperQtiTransformer}
import play.api.libs.json.{Json, JsObject}

import scala.xml.transform.RewriteRule
import scala.xml.{Node, Elem}

/**
  * PARCC allows 1 point per part but only for items that have the following metadata set to 1.
  * The “TwoPointScoring” is a misnomer as the items can have up to 4 parts and be worth 4 points.
  *
  * <imsmd:parccTwoPointScoring>1</imsmd:parccTwoPointScoring>
  *
  */

object KDSMode extends Enumeration {
  type Mode = Value
  val SBAC,PARCC = Value
}


private[keydatasys] class KDSQtiTransformer(mode:KDSMode.Mode) extends SuperQtiTransformer with ProcessingTransformer {

  override def normalizeScore(resource:Node) = {
    val twoPointKey = s"${mode.toString.toLowerCase}TwoPointScoring"
    val twoPointFlag : String = (resource \ "metadata" \ "lom" \ "general" \ twoPointKey).text.trim
    twoPointFlag == "1"
  }

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