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
  val SBAC,PARCC = Value
}


object KdsType {
  def apply(resource: Node) : Option[KdsType] = {

    try {
      val itemTypeId = (resource \ "metadata" \ "lom" \ "general" \ "itemTypeId").text.trim.toInt
      itemTypeId match {
        case MultiPart.id => Some(MultiPart)
        case Ebsr.id => Some(Ebsr)
      }
    } catch {
      case t:Throwable => None
    }
  }
}

sealed abstract class KdsType(val id:Int) {
  def isType(resource:Node) = {
    (resource \ "metadata" \ "lom" \ "general" \ "itemTypeId").text.trim == this.id.toString
  }
}

case object MultiPart extends KdsType(8)
case object Ebsr extends KdsType(11)



private[keydatasys] class KDSQtiTransformer(mode:KDSMode.Mode) extends SuperQtiTransformer with ProcessingTransformer {

  private def isEbsrItem(resource:Node) =
    (resource \ "metadata" \ "lom" \ "general" \ "itemTypeId").text.trim == "11"

  private def isParccTwoPointScoring(resource:Node) = {
    if(mode == KDSMode.SBAC){
      false
    } else {
      val FLAG = s"parccTwoPointScoring"
      val twoPointFlag : String = (resource \ "metadata" \ "lom" \ "general" \ FLAG).text.trim
      twoPointFlag == "1"
    }
  }

  override def normalizeDenominator(resource:Node, qti: Node) = {

    KdsType(resource).map { t =>

      
    }
    val js = toJs(qti).map( j =>
      j.responseVars.length
    )

    val ebsrItem = isEbsrItem(resource)
    val twoPointScoring = isParccTwoPointScoring(resource)
    ebsrItem || twoPointScoring
    //TODO:
    None
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