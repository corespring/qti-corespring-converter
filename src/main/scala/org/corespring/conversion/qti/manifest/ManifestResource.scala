package org.corespring.conversion.qti.manifest

case class ManifestResource(
                             path: String,
                             resourceType: ManifestResourceType.Value,
                             inline: Boolean
                           ) {
  def is(resourceType: ManifestResourceType.Value) = this.resourceType == resourceType
}
