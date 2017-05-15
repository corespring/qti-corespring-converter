package org.corespring.conversion.qti

import scala.collection.JavaConversions._
import java.io.File
import java.util.zip.ZipFile

import com.progresstesting.conversion.zip.{NewProgressTestingQtiZipConverter, ProgressTestingQtiZipConverter}
import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions

import scala.concurrent.Await.result
import scala.concurrent.duration._

class TransformProgressTestingSpec extends Specification with NoTimeConversions{

  val zipName = "/progress-testing-qti-8409.zip"
  //val zipName = "/full.zip"

  "transform" should {
    "run" in {
      val zip = new ZipFile(this.getClass.getResource(zipName).getPath)
      val newPath = "target/out-new.zip"
      val oldPath = "target/out-old.zip"
      val metadata = None
      val out = result(NewProgressTestingQtiZipConverter.convert(zip, newPath, metadata), 4.seconds)
      val oldOut = result(ProgressTestingQtiZipConverter.convert(zip, oldPath, metadata), 4.seconds)

      val newArray = out.entries().toArray.map(_.getName).sorted
      val oldArray = oldOut.entries().toArray.map(_.getName).sorted


      forall(newArray){
        n =>
          val e  = oldOut.getEntry(n)
          e must_!= null
      }

      forall(oldArray){
        n =>
          val e  = out.getEntry(n)
          e must_!= null
      }
    }
  }
}
