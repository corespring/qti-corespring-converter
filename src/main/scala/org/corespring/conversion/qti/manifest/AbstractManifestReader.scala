package org.corespring.conversion.qti.manifest

import org.corespring.common.file.SourceWrapper

abstract class AbstractManifestReader {

  def read(manifest: SourceWrapper, sources: Map[String, SourceWrapper]): QTIManifest

}