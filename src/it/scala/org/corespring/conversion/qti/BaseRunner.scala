package org.corespring.conversion.qti

import java.io.File
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipFile}

import com.keydatasys.conversion.qti.KDSMode
import org.apache.commons.io.IOUtils
import org.slf4j.LoggerFactory
import org.specs2.mutable.Specification
import play.api.libs.json.Json

import scala.collection.convert.ImplicitConversions.`enumeration AsScalaIterator`

trait BaseRunner extends Specification {


  val logger = LoggerFactory.getLogger(this.getClass)


  private def mkTmpDir(prefix: String = "run-helper") = Files.createTempDirectory(s"$prefix")

  private def buildZip(dir: Path, sourceId: String, itemTypeId: String, parccTwoPointScoring: Boolean, partsCount: Int, mode: KDSMode.Mode): Path = {
    val manifest = Info("imsmanifest.xml",
      ManifestMaker.resource(
        sourceId,
        itemTypeId,
        parccTwoPointScoring,
        partsCount
      ).toString)

    val qtiName = s"${mode.toString.toLowerCase()}.xml"
    val r = this.getClass.getResource(s"/$sourceId/$qtiName")
    val contents = scala.io.Source.fromFile(r.toURI, "utf8").getLines().mkString("\n")
    val qti = Info(s"$sourceId.xml", contents)
    QtiZipWriter.write(dir.resolve(s"$sourceId.zip"), manifest, qti)
  }

  private def run(input: String,
                  output: String,
                  vendor: String,
                  sourceId: String,
                  metadata: String = "{}") = {
    Runner.main(Array(
      "--input", input,
      "--vendor", vendor,
      "--limit", "0",
      "--sourceId", sourceId,
      "--output", output,
      "--killRuntime", "false",
      "--metadata", metadata
    ))
  }

  def convert(sourceId: String, itemTypeId: String, mode: KDSMode.Mode, partsCount: Int, twoPointScoring: Boolean) = {

    val tmpDir = mkTmpDir(s"kds-$mode-$sourceId")
    val convertedPath = tmpDir.resolve(s"output.zip")
    val archive: Path = buildZip(
      tmpDir,
      sourceId,
      itemTypeId,
      twoPointScoring && mode == KDSMode.PARCC,
      partsCount,
      mode)
    run(
      archive.toString,
      convertedPath.toString,
      "kds",
      sourceId,
      s"""{"scoringType": "$mode"}"""
    )
    val zip = new ZipFile(new File(convertedPath.toString))

    logger.info(s"path: ${convertedPath}, entries: ${zip.entries.toArray.map(_.getName).mkString(",")}")

    zip

  }

  private def json(zip: ZipFile, e: ZipEntry) = {
    val jsonString = IOUtils.toString(zip.getInputStream(e))
    Json.parse(jsonString)
  }

  def getPlayerDef(zip: ZipFile) = {

    val entry = zip.entries.find {
      e =>
        logger.info(s"e.getName: ${e.getName}")
        e.getName.contains("player-definition.json")
    }

    entry.map(e => json(zip, e))

  }

}
