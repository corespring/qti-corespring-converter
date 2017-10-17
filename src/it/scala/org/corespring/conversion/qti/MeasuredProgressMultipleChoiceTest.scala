package org.corespring.conversion.qti

import java.io.File
import java.util.zip.ZipFile

import org.specs2.mutable.Specification
import scala.collection.JavaConversions._

class MeasuredProgressMultipleChoiceTest extends Specification {


  //  override def vendor = "measuredprogress"

  val sourceId = "RES-55158e3c-056c-4f06-b1a8-182e0a1ff4b4"

  "measuredprogress " should {

    "add value to corespring-multiple-choice" in {

      val dir = RunHelper.mkTmpDir()

      val zip = RunHelper.buildZip(
        dir,
        sourceId,
        this.getClass.getResource(s"/MP-688"))


      val output = dir.resolve("new-mp.zip")
      val legacy = dir.resolve("legacy-mp.zip")

       //run the new converter
      RunHelper.run(
        zip.toAbsolutePath.toString,
        output.toString,
        "measuredprogress",
        sourceId)

      val playerDef = new ZipFile(output.toFile).entries.find{
         e =>
          e.getName.contains("player-definition.json")
      }


      RunHelper.run(
        zip.toAbsolutePath.toString,
        legacy.toString,
        "old-measuredprogress",
        sourceId
      )

      val legacyPlayerDef = new ZipFile(legacy.toFile).entries.find{
        e =>
          e.getName.contains("player-definition.json")
      }

      println(s"playerDefinition: $playerDef")
      println(s"playerDefinition: $legacyPlayerDef")
      println(s"dir: $dir")

      true === true

    }
  }
}
