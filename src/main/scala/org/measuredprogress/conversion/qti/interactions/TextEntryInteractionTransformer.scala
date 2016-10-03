package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.interactions.{TextEntryInteractionTransformer => CoreSpringTextEntryInteractionTransformer}

import scala.xml.Node

class TextEntryInteractionTransformer(qti: Node) extends CoreSpringTextEntryInteractionTransformer(qti) {

  override def correctResponses(node: Node, qti: Node) = {
    val response = responseDeclaration(node, qti)
    ((response \ "correctResponse" \\ "value").map(_.text) ++
      (response \ "mapping" \\ "mapEntry").filter(mapEntry => mapEntry.attribute("mappedValue") match {
        case Some(attribute) => attribute.text.toFloat == 1F
        case _ => false
      }).map(_.attribute("mapKey").map(_.text)).flatten).toSet
  }


}
