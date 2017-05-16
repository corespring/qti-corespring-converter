package org.corespring.common

import play.api.libs.json.JsValue

case class CorespringItem( id:String,
                           playerDefinition: JsValue,
                           profile: JsValue,
                           assets: Seq[String])