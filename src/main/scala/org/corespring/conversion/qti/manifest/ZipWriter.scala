package org.corespring.conversion.qti.manifest

import java.io.{File, FileInputStream, FileOutputStream}
import java.util.zip.{ZipEntry, ZipOutputStream}

import org.apache.commons.io.IOUtils

object ZipWriter {
  def compressDir(src: File, outputFile: String) = {
    val zipFile = new ZipOutputStream(new FileOutputStream(outputFile))
    compressDirectoryToZipfile(src, src, zipFile)
    IOUtils.closeQuietly(zipFile)
  }

  private def compressDirectoryToZipfile(
                                          rootDir: File,
                                          sourceDir: File,
                                          out: ZipOutputStream): Unit = {
    sourceDir.listFiles().map { file =>
      if (file.isDirectory()) {
        compressDirectoryToZipfile(rootDir, new File(file.getAbsolutePath()), out)
      } else {
        val name = file.getCanonicalPath().replace(s"${rootDir.getCanonicalPath()}/", "")
        val entry = new ZipEntry(name)
        out.putNextEntry(entry)
        val in = new FileInputStream(file)
        IOUtils.copy(in, out)
        IOUtils.closeQuietly(in)
      }
    }
  }
}
