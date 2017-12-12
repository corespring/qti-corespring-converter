package org.corespring.conversion.zip

import java.util.zip._

import org.corespring.common.util._
import play.api.libs.json._

import scala.concurrent.Future

case class ConversionOpts(limit: Int = 0, sourceIds: Seq[String] = Seq.empty)

/**
  * Represents an interface which can translate a QTI zip file into a CoreSpring JSON zip file
  */
trait QtiToCorespringConverter extends UnicodeCleaner {


  def convert(
               zip: ZipFile,
               path: String,
               metadata: Option[JsObject],
               opts: ConversionOpts = ConversionOpts()
             ): Future[ZipFile]

}
