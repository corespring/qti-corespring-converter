package com.keydatasys.conversion.qti.manifest

import java.util.zip.ZipFile

import org.corespring.common.file.SourceWrapper
import org.corespring.conversion.qti.manifest.{QTIManifest, ManifestReader => QTIManifestReader}

object ManifestReader extends ManifestFilter {

  val filename = "imsmanifest.xml"

  def read(file: SourceWrapper, sources: Map[String, SourceWrapper]): QTIManifest =
    QTIManifestReader.read(filterManifest(file), sources)

//  def read(file: SourceWrapper, zip: ZipFile): QTIManifest =
//    QTIManifestReader.read(filterManifest(file), zip)

}
