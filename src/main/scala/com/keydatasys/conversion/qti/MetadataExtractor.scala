package com.keydatasys.conversion.qti

import play.api.libs.json._

object MetadataExtractor {

  def sourceIdObj(id: String) = {
    Json.obj("sourceId" -> id)
  }

}

