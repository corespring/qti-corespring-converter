package org.corespring.conversion.qti.manifest

import java.util.zip.ZipFile

import com.keydatasys.conversion.qti.util.PassageScrubber
import org.apache.commons.io.IOUtils
import org.corespring.common.util.EntityEscaper
import org.corespring.macros.DescribeMacro.describe
import org.corespring.utils.ErrorDir
import org.slf4j.LoggerFactory

import scala.xml.{Node, XML}

object ZipReader extends PassageScrubber with EntityEscaper {

  import org.corespring.utils.CDataHelper

  lazy val logger = LoggerFactory.getLogger(ZipReader.this.getClass)


  def fileContents(zip: ZipFile, name: String): Option[String] = {
    val entry = zip.getEntry(name)
    if (entry == null) {
      None
    } else {
      val is = zip.getInputStream(entry)
      val s = IOUtils.toString(is, "UTF-8")
      IOUtils.closeQuietly(is)
      Some(s)
    }
  }

  def fileXML(zip: ZipFile, name: String): Option[Node] = fileContents(zip, name)
    .flatMap { s =>
      val cleaned = escapeEntities(CDataHelper.stripCDataAndEscapeIfNeeded(s))
      try {
        val xmlOut = XML.loadString(cleaned)
        Some(xmlOut)
      }
      catch {
        case e: Exception => {
          logger.error(s"Error reading $name, message: ${e.getMessage}")
          ErrorDir.dump(name, Some(e), "cleaned.xml" -> cleaned, "raw.xml" -> s)
          logger.trace(describe(s, cleaned))
          None
        }
      }
    }
}
