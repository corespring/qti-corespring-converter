package org.corespring.common.xml

import java.io.InputStream

import scala.io.Source
import scala.xml.Node
import scala.xml.parsing.ConstructingParser

/**
  * A ConstructingParser with Xhtml entities.
  *
  * Until we move to scala > 2.11 we can use this.
  * Thereafter we can use scala-xml instead.
  */
object XhtmlParser {

  private def loadSource(s:Source) : Node = {
    val p = ConstructingParser.fromSource(s, true)
    p.ent ++= XhtmlEntities()
    p.document().docElem
  }

  def loadString(s:String) : Node = loadSource(Source.fromString(s))

  def loadStream(is:InputStream) : Node = loadSource(Source.fromInputStream(is))
}
