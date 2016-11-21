package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.{GraphicGapMatchInteractionTransformer => CoreSpringGraphicGapMatchInteractionTransformer}

object GraphicGapMatchInteractionTransformer extends CoreSpringGraphicGapMatchInteractionTransformer {
  override val DefaultSnapEnabled = true
  override val DefaultChoiceAreaPosition = "bottom"
}
