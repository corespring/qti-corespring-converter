package org.corespring.conversion.qti

import java.io.{File, FileInputStream, FileOutputStream}
import org.corespring.conversion.qti.manifest.{QTIManifest}
import org.houghtonmifflinharcourt.conversion.qti.interactions.{ChoiceInteractionTransformer, ExtendedTextInteractionTransformer}
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.xml.XML
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.zip.{ZipEntry, ZipFile, ZipInputStream, ZipOutputStream}

import org.apache.commons.io.IOUtils
import org.apache.commons.io.FilenameUtils

case class RunOpts(
                    input: String,
                    output: String,
                    mode: String
                  )


object Run extends App {

  val version = {

  }
  var jsonResult: Map[String, JsObject] = Map.empty
  println(qtiConverter.BuildInfo.toString)

  val logger = LoggerFactory.getLogger(Run.this.getClass)

  val parser = new scopt.OptionParser[RunOpts]("run") {
    head("run", "version")


    opt[String]('i', "input").required().action((i, c) => c.copy(input = i))

    opt[String]('o', "output").required().action((o, c) => c.copy(output = o))

    opt[String]('m', "mode").required().action( (o,c) => c.copy(mode = o))


  }

  parser.parse(args, RunOpts("", "", "")) match {
    case Some(runOpts) => {
      logger.info(s"${runOpts}")
      // var outDirectoryNew:String = s"pie/output/new/"
      if(runOpts.mode == "single-file"){
        val inPath = s"pie/input/" + runOpts.input;
        val outPath = s"pie/output/" + runOpts.output;
        // outDirectoryNew = outFile.getParent() + "/new/"

        val  outDirectoryNew = Files.createTempDirectory("qti-conversion")

        //val  out = new ZipOutputStream(new FileOutputStream(outPath))
        val zis: ZipInputStream = new ZipInputStream(new FileInputStream(inPath));
        //get the zipped file list entry
        var ze: ZipEntry = zis.getNextEntry();
        while (ze != null) {
          val fileName = ze.getName()
          val zip = new ZipFile(inPath)
          val xmlData=IOUtils.toString(zip.getInputStream(zip.getEntry(fileName)), StandardCharsets.UTF_8)
          //val a= xmlData
          val xml = XML.loadString(xmlData)
          if( (xml \\ "choiceInteraction").length > 0 ) {
            jsonResult = ChoiceInteractionTransformer.interactionJs(xml, QTIManifest.EmptyManifest)
            logger.info("pie json for choiceInteraction")
          }
          else if( (xml \\ "extendedTextInteraction").length > 0 ) {
            jsonResult = ExtendedTextInteractionTransformer.interactionJs(xml, QTIManifest.EmptyManifest)
            logger.info("pie json for extentedTextInteraction")
          }
          val outFile = new File(outPath)

          val outDirectory = Paths.get(outDirectoryNew.toString(), FilenameUtils.removeExtension(fileName) +".json")
          val json = Json.prettyPrint(jsonResult.head._2);
          val basePath = Paths.get(s"${outDirectory}")
          Files.write(basePath,json.getBytes(StandardCharsets.UTF_8))
          logger.info("file successfully converted into pie json!")
          ze = zis.getNextEntry()
        }
        zis.closeEntry()
        zis.close()

        val  out = new ZipOutputStream(new FileOutputStream(outPath))
        outDirectoryNew.toFile().listFiles().map { file =>
          val entry = new ZipEntry(file.getName())
          out.putNextEntry(entry)
          val in = new FileInputStream(file)
          IOUtils.copy(in, out)
          IOUtils.closeQuietly(in)
        }
        IOUtils.closeQuietly(out)
      } else {
        logger.info("TODO...")
      }
    }
    case None => {
      println(parser.usage)
      //sys.exit(1)
    }
  }
}
