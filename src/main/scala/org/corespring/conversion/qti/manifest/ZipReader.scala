package org.corespring.conversion.qti.manifest

import java.io.InputStream
import java.util.zip.ZipFile

import org.apache.commons.io.IOUtils
import org.corespring.common.xml.{XMLNamespaceClearer, XhtmlParser}
import org.slf4j.LoggerFactory

import scala.xml.Node

object ZipReader  {

  lazy val logger = LoggerFactory.getLogger(ZipReader.this.getClass)

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
      * Note: It is important that we use the XhtmlParser.
      * The default scala parser escapes CDATA contents.
      */
    stream(zip, name).map { s =>
      val e = XhtmlParser.loadStream(s)
      val noNamespace = XMLNamespaceClearer.clearNamespace(e)
      IOUtils.closeQuietly(s)
      noNamespace
    }
  }
}
