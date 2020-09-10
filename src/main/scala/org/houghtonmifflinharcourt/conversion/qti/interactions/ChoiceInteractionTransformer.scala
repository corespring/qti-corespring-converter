package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.common.util.XHTMLCleaner
import org.corespring.conversion.qti.interactions.pie.InteractionTransformer
import org.corespring.conversion.qti.interactions.pie.{ChoiceInteractionTransformer => CorespringChoiceInteractionTransformer}
import play.api.libs.json._

import scala.xml._
import scala.xml.transform._

object ChoiceInteractionTransformer extends InteractionTransformer with XHTMLCleaner {

  override def interactionJs(qti: Node, manifest: Node) =
    CorespringChoiceInteractionTransformer.interactionJs(qti, manifest)
}
