package org.corespring.conversion.qti.interactions

import scala.xml.Elem

object FoldableInteractionTransformer extends NodeReplacementTransformer {

  def labelToReplace: String = "foldable"
  def replacementNode: Elem = <div corespring-foldable="corespring-foldable"></div>

}
