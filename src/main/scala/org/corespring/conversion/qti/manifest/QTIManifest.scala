package org.corespring.conversion.qti.manifest

case class QTIManifest(items: Seq[ManifestItem] = Seq.empty, otherFiles: Seq[String] = Seq.empty)