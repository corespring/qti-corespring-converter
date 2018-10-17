package org.corespring.conversion.qti

import java.io.FileOutputStream
import java.nio.file.Path
import java.util.zip.{ZipEntry, ZipOutputStream}

case class Info(path: String, contents: String)

object QtiZipWriter {

  def write(destination: Path, files: Info*) = {

    val fileOut = destination.toFile
    val zipOut = new ZipOutputStream(new FileOutputStream(fileOut))
    files.foreach(i => {
      val e = new ZipEntry(i.path)
      zipOut.putNextEntry(e)
      val data: Array[Byte] = i.contents.getBytes
      zipOut.write(data, 0, data.length)
      zipOut.closeEntry
    })
    zipOut.close
    destination
  }
}