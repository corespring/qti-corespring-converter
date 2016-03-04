package org.corespring.common.file

import java.io.{ByteArrayOutputStream, FileInputStream, File, InputStream}
import java.nio.charset.CodingErrorAction

import org.apache.commons.io.FileUtils

import scala.io.{Source, Codec}


/**
 * Caches the result of a Source's getLines into a temporary file buffer. Then provides interfaces for obtaining Source
 * objects from the cached file in future. File is removed when the JVM halts.
 */
case class SourceWrapper(name: String, inputStream: InputStream) {

  def prefix = s"source-wrapper-${inputStream.hashCode}"
  val suffix = ".tmp"

  var tempFile: Option[File] = None

  private def getFile: File = tempFile match {
    case Some(file) => file
    case _ => {
      val file = File.createTempFile(prefix, suffix)
      file.deleteOnExit()
      FileUtils.copyInputStreamToFile(inputStream, file)
      tempFile = Some(file)
      file
    }
  }

  val utf8 = List("txt", "xml", "json")

  private def inferCodec: Codec = Codec(if (utf8.contains(name.split("\\.").last)) "UTF-8" else "ISO-8859-1")

  /**
   * Creates a Source object from the lines contained within the file. Provided for compatibility with APIs that require
   * a Source object.
   */
  def toSource(codec: Codec = inferCodec) = {
    codec.onMalformedInput(CodingErrorAction.IGNORE)
    codec.onUnmappableCharacter(CodingErrorAction.IGNORE)
    Source.fromFile(getFile)(codec)
  }

  private val UTF8_BOM = "\uFEFF"

  private def removeUTF8BOM(s: String) = s.startsWith(UTF8_BOM) match {
    case true => s.substring(1);
    case _ => s
  }

  def mkString = removeUTF8BOM(getLines.mkString("\n").trim)

  def toByteArray: Array[Byte] = {
    val input = new FileInputStream(getFile)
    val output = new ByteArrayOutputStream()
    var buffer = new Array[Byte](65536)
    var l = input.read(buffer)
    while (l > 0) {
      output.write (buffer, 0, l)
      l = input.read(buffer)
    }
    input.close()
    output.close()
    output.toByteArray()
  }

  def getLines = toSource("UTF-8").getLines

}