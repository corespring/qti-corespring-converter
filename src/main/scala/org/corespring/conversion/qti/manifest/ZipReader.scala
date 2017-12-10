package org.corespring.conversion.qti.manifest

import java.io.InputStream
import java.util.zip.ZipFile

import org.apache.commons.io.IOUtils
import org.slf4j.LoggerFactory

import scala.io.Source
import scala.xml.Node
import scala.xml.parsing.ConstructingParser

object ZipReader  {

  lazy val logger = LoggerFactory.getLogger(ZipReader.this.getClass)

  private def stripCDataTags(xmlString: String) =
    """(?s)<!\[CDATA\[(.*?)\]\]>""".r.replaceAllIn(xmlString, "$1")


  def stream(zip:ZipFile, name:String) : Option[InputStream] = {
    val entry = zip.getEntry(name)
    if(entry == null){
      None
    } else {
      Some(zip.getInputStream(entry))
    }
  }

  def qti(zip:ZipFile, name:String) = xml(zip, name)

  def xml(zip:ZipFile, name:String) : Option[Node] = {
    /**
      * Note: It is important that we use the ConstructingParser.
      * The default scala parser escapes CDATA contents.
      */
    stream(zip, name).map{ s =>
      val parser = ConstructingParser.fromSource(Source.fromInputStream(s, "UTF-8"), true)
      val e = parser.document().docElem
      IOUtils.closeQuietly(s)
      e
    }
  }

//  def fileXML(zip: ZipFile, name:String) : Option[Node] = fileContents(zip, name)
//    .flatMap{ s =>
//      val cleaned = stripCDataTags(s)
//      val escaped = escapeEntities(cleaned)
//      val scrubbed = scrub(escaped)
//      try {
//        val xmlOut = XML.loadString(scrubbed)
//        Some(xmlOut)
//      } catch {
//        case e :Exception => {
//          logger.error(s"Error reading $name, message: ${e.getMessage}")
//          e.printStackTrace()
//          if(logger.isDebugEnabled()){
//            //e.printStackTrace()
//          }
//          ErrorDir.dump(
//            name,
//            Some(e),
//            "content.xml" -> s,
//            "cleaned.xml" -> cleaned,
//            "escaped.xml" -> escaped,
//            "scrubbed.xml" -> scrubbed
//          )
//
//          None
//        }
//      }
//    }
}
