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

/**
  * The part count can be determined by looking at the <parts> node in the manifest here:
  *
  *
  *
  * <parts>
  *
  * <part itemTypeId="19"/>
  *
  * <part itemTypeId="12"/>
  *
  * <part itemTypeId="10"/>
  *
  * <part itemTypeId="12"/>
  *
  * </parts>
  *
  *
  *
  * The part count should always be used for normalization
  * because the student is expected to get all interactions correct for the part.
  *
  * Only Multi-Part (itemTypeId = 8)
  * and EBSR
  * (itemTypeId = 11)
  * items will have a parts node. This is true for both PARCC and SBAC.
  *
  */

object KDSMode extends Enumeration {
  type Mode = Value
  val SBAC, PARCC = Value
}

private[keydatasys] class KDSQtiTransformer(mode: KDSMode.Mode) extends SuperQtiTransformer with ProcessingTransformer {

  private def isEbsr(resource: Node) =
    (resource \ "metadata" \ "lom" \ "general" \ "itemTypeId").text.trim == "11"

  private def isMultiPart(resource: Node) =
    (resource \ "metadata" \ "lom" \ "general" \ "itemTypeId").text.trim == "8"

  private def isParccTwoPointScoring(resource: Node) = {
    if (mode == KDSMode.SBAC) {
      false
    } else {
      val FLAG = s"parccTwoPointScoring"
      val twoPointFlag: String = (resource \ "metadata" \ "lom" \ "general" \ FLAG).text.trim
      twoPointFlag == "1"
    }
  }

  private def shouldNormalize(resource: Node): Boolean = isEbsr(resource) ||
    isMultiPart(resource) ||
    isParccTwoPointScoring(resource)

  private def partsCount(resource: Node) = {
    val count = (resource \ "metadata" \ "lom" \ "parts" \\ "part").length
    if (count > 0) Some(count) else None
  }

  override def normalizeDenominator(resource: Node, qti: Node): Option[Int] = {
    lazy val defaultDenominator: Option[Int] = toJs(qti).map(j => j.responseVars.length)
    if(isMultiPart(resource) || isEbsr(resource)){
      Some(1)
    } else if (isParccTwoPointScoring(resource)){
      partsCount(resource).orElse(defaultDenominator)
    } else {
      defaultDenominator
    }
  }

  override def ItemBodyTransformer = new RewriteRule with XMLNamespaceClearer {

    override def transform(node: Node): Seq[Node] = {
      node match {
        case elem: Elem if elem.label == "itemBody" => {
          <div class="item-body kds qti">
            {elem.child}
          </div>
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