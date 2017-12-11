package org.corespring.conversion.qti

import ManifestUtils.buildItem

class AudioInBodySpec {
  val item = buildItem("""<![CDATA[<audio controls><source type="audio/mp3" src="foo.mp3"></audio>]]>""")
  val builder = new Builder()
  val zip = builder.addItem("1", item).build()
}
