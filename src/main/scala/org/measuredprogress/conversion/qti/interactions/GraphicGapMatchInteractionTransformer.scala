package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.{GraphicGapMatchInteractionTransformer => CoreSpringGraphicGapMatchInteractionTransformer}
import play.api.libs.json.Json

import scala.xml.Node

object GraphicGapMatchInteractionTransformer extends CoreSpringGraphicGapMatchInteractionTransformer {
  override val DefaultSnapEnabled = true
  override val DefaultChoiceAreaPosition = "bottom"
}
