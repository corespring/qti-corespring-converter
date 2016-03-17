package org.parcconline.conversion.qti.util

import org.corespring.common.file.SourceWrapper
import org.corespring.conversion.qti.manifest.ManifestResource

trait PassageTransformer {

  def transformPassage(resource: ManifestResource)(implicit sources: Map[String, SourceWrapper]): Option[String] = {
    None
  }

}