package com.keydatasys.conversion.qti

import com.keydatasys.conversion.qti.interactions.{ChoiceInteractionTransformer => KDSChoiceInteractionTransformer, GraphicGapMatchInteractionTransformer => KDSGraphicGapMatchInteractionTransformer, MatchInteractionTransformer => KDSMatchInteractionTransformer, TextEntryInteractionTransformer => KDSTextEntryInteractionTransformer, _}
import com.keydatasys.conversion.qti.processing.ProcessingTransformer
import org.corespring.common.xml.XMLNamespaceClearer
import org.corespring.conversion.qti.interactions._
import org.corespring.conversion.qti.{QtiTransformer => SuperQtiTransformer}
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsObject, Json}

import scala.xml.transform.RewriteRule
import scala.xml.{Elem, Node}
import org.corespring.macros.DescribeMacro._

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

sealed abstract class ItemType(val id:String)
case object MULTIPART extends ItemType("8")
case object EBSR extends ItemType("11")

private[keydatasys] class KDSQtiTransformer(mode: KDSMode.Mode) extends SuperQtiTransformer with ProcessingTransformer {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private def itemTypeId(resource:Node) = {
    val id = (resource \ "metadata" \ "lom" \ "general" \ "itemTypeId").text.trim
    logger.trace(describe(id, resource))
    id
  }
  private def isEbsr(resource: Node) = itemTypeId(resource) == "11"

  private def isMultiPart(resource: Node) = itemTypeId(resource) == "8"

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

    if(resource.label.trim != "resource"){
      throw new Exception(s"You must pass in a <resource> node but got: ${resource.label.trim}")
    }

    if(mode == KDSMode.SBAC){
      Some(1)
    } else {
      lazy val defaultDenominator: Option[Int] = toJs(qti).map(j => j.responseVars.length)
      if (shouldNormalize(resource)) {
        val count = partsCount(resource)
        logger.debug(s"[normalizeDenominator] should normalize using parts count: ${count}")
        count.orElse(defaultDenominator)
      } else {
//        logger.debug(s"[normalizeDenominator] should not normalize using parts count, isEbsr: ${isEbsr(resource)} isMultiPart: ${isMultiPart(resource)} isParccTwoPointScoring: ${isParccTwoPointScoring(resource)}")
        defaultDenominator
      }

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