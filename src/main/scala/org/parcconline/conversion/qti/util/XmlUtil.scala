package org.parcconline.conversion.qti.util

object XmlUtil {

  private val xmlDeclarationRegex = "\\s*<\\?xml.*\\?>(.*)".r

  def removeXmlDeclaration(string: String): String = string match {
    case xmlDeclarationRegex(doc) => doc
    case _ => string
  }

}
