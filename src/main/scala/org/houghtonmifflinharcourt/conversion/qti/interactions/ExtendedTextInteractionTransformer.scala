package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.common.util.XHTMLCleaner
import org.corespring.conversion.qti.interactions.pie.{InteractionTransformer, ExtendedTextInteractionTransformer => CorespringExtendedTextInteractionTransformer}

import scala.xml._

object ExtendedTextInteractionTransformer extends InteractionTransformer with XHTMLCleaner {

  override def interactionJs(qti: Node, manifest: Node) =
    CorespringExtendedTextInteractionTransformer.interactionJs(qti, manifest)
}
