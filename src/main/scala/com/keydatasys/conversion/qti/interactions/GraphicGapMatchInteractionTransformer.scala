package com.keydatasys.conversion.qti.interactions

import org.corespring.common.util.NumberParsers
import org.corespring.conversion.qti.interactions.InteractionTransformer
import play.api.libs.json._
import org.corespring.conversion.qti.interactions.{GraphicGapMatchInteractionTransformer => CoreSpringGraphicGapMatchInteractionTransformer}

import scala.xml._

class GraphicGapMatchInteractionTransformer extends CoreSpringGraphicGapMatchInteractionTransformer {
  override val DefaultSnapEnabled = true
}
