package org.corespring.conversion.qti.manifest

import scala.xml.Node

case class ManifestItem(id: String, filename: String, resources: Seq[ManifestResource] = Seq.empty, manifest: Node)

