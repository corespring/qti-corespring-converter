package org.corespring.conversion.qti.manifest

import java.util.zip.ZipFile

import org.corespring.common.file.SourceWrapper

case class ManifestResource(path: String, resourceType: ManifestResourceType.Value) {
  def is(resourceType: ManifestResourceType.Value) = this.resourceType == resourceType
}
