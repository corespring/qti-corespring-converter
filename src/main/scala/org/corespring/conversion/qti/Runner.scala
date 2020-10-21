package org.corespring.conversion.qti

import java.io.{File, FileInputStream, FileOutputStream}

import org.corespring.conversion.qti.manifest.QTIManifest
import org.houghtonmifflinharcourt.conversion.qti.interactions.{HotspotInteractionTransformer,AssociateInteractionTransformer,ChoiceInteractionTransformer, ExtendedTextInteractionTransformer, MatchInteractionTransformer, InlineChoiceInteractionTransformer,OrderInteractionTransformer,HottextInteractionTransformer,TextEntryInteractionTransformer}
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.xml.XML
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.util.zip.{ZipEntry, ZipFile, ZipInputStream, ZipOutputStream}

import org.apache.commons.io.IOUtils
import org.apache.commons.io.FilenameUtils
import org.corespring.qti.models.interactions.InlineChoiceInteraction

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
  def getListOfFiles(dir: String,extensions:  List[String]):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList.filter { file =>
        extensions.exists(file.getName.endsWith(_))
      }
    } else {
      List[File]()
    }
  }
  parser.parse(args, RunOpts("", "", "")) match {
    case Some(runOpts) => {
      logger.info(s"${runOpts}")
      if(runOpts.mode == "single-file"){
        val inPath = s"pie/input/";
        val outPath = s"pie/output/" + runOpts.output;
        val files:List[File] = getListOfFiles(inPath.toString(),List("zip"))
        val  tempDir = Files.createTempDirectory("qti-conversion")
        val outDirectory = Paths.get(tempDir.toString(), "output.jsonl")
        var mcCount = 0;
        var extCount = 0;
        var matchCount = 0;
        var icCount = 0;
        var htCount = 0;
        var ocCount = 0;
        var teCount = 0;
        var mlCount = 0;
        var hsCount = 0
        for (fname <- files) {
          val zis: ZipInputStream = new ZipInputStream(new FileInputStream(fname));
          var fileName = "";
          //get the zipped file list entry
          var ze: ZipEntry = zis.getNextEntry();
          while (ze != null) {
            try {
              jsonResult = Map.empty;
              fileName = ze.getName()
              if (fileName != "imsmanifest.xml" && FilenameUtils.isExtension(fileName, "xml")) {

                val zip = new ZipFile(fname)
                //val xmlData = IOUtils.toString(zip.getInputStream(zip.getEntry(fileName)), StandardCharsets.UTF_8)
                val xml = XML.load(zip.getInputStream(zip.getEntry(fileName))) //.loadString(xmlData)

                if ((xml \\ "choiceInteraction").length > 0) {
                  jsonResult = ChoiceInteractionTransformer.interactionJs(xml, QTIManifest.EmptyManifest)
                  //logger.info("pie json for choiceInteraction")
                  mcCount += 1;
                }
                else if ((xml \\ "extendedTextInteraction").length > 0) {
                  jsonResult = ExtendedTextInteractionTransformer.interactionJs(xml, QTIManifest.EmptyManifest)
                  //logger.info("pie json for extentedTextInteraction")
                  extCount += 1;
                }
                else if ((xml \\ "matchInteraction").length > 0) {
                  jsonResult = MatchInteractionTransformer.interactionJs(xml, QTIManifest.EmptyManifest)
                  //logger.info("pie json for matchInteraction")
                  matchCount += 1;
                }
                else if ((xml \\ "inlineChoiceInteraction").length > 0) {
                  jsonResult = InlineChoiceInteractionTransformer.interactionJs(xml, QTIManifest.EmptyManifest)
                  //logger.info("pie json for inlineChoiceInteraction")
                  icCount += 1;
                }
                else if ((xml \\ "orderInteraction").length > 0) {
                  jsonResult = OrderInteractionTransformer.interactionJs(xml, QTIManifest.EmptyManifest)
                  //logger.info("pie json for orderInteraction")
                  ocCount += 1;
                }
                else if ((xml \\ "hottextInteraction").length > 0) {
                  jsonResult = HottextInteractionTransformer.interactionJs(xml, QTIManifest.EmptyManifest)
                  //logger.info("pie json for HottextInteraction")
                  htCount += 1;
                }
                else if ((xml \\ "textEntryInteraction").length > 0) {
                  jsonResult = TextEntryInteractionTransformer.interactionJs(xml, QTIManifest.EmptyManifest)
                  //logger.info("pie json for TextEntryInteraction")
                  teCount += 1;
                }
                else if ((xml \\ "associateInteraction").length > 0) {
                  jsonResult = AssociateInteractionTransformer.interactionJs(xml, QTIManifest.EmptyManifest)
                  //logger.info("pie json for associateInteraction")
                  mlCount += 1;
                }
                else if ((xml \\ "hotspotInteraction").length > 0) {
                  jsonResult = HotspotInteractionTransformer.interactionJs(xml, QTIManifest.EmptyManifest)
                  //logger.info("pie json for hotspotInteraction")
                  hsCount += 1;
                }
                if (jsonResult.size > 0) {
                  val json = Json.stringify(jsonResult.head._2);
                  val basePath = Paths.get(s"${outDirectory}")
                  Files.write(basePath, (json + System.lineSeparator()).getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.APPEND)
                  //logger.info("file successfully converted into pie json!")
                }
              }
              ze = zis.getNextEntry()
            } catch {
              // Case statement-2
              case x: Exception => {

                // Displays this if input/output
                // exception is found
                ze = zis.getNextEntry()
                println(x.getMessage + fileName +" "+ fname)

              }
            }
          }
          zis.closeEntry()
          zis.close()
          logger.info(fname + " file successfully converted into pie json!")
        }
        val  out = new ZipOutputStream(new FileOutputStream(outPath))
        tempDir.toFile().listFiles().map { file =>
          val entry = new ZipEntry(file.getName())
          out.putNextEntry(entry)
          val in = new FileInputStream(file)
          IOUtils.copy(in, out)
          IOUtils.closeQuietly(in)
        }
        IOUtils.closeQuietly(out)
        logger.info("Choicenteraction Count: " + mcCount + " ExtendedTextInteraction Count " + extCount + " MatchInteraction Count " + matchCount + " InlineChoiceInteraction Count " + icCount + " OrderInteraction Count " + ocCount+ " HottextInteraction Count " + htCount+ " TextEntryInteraction Count " + teCount+ " associateInteraction Count "+mlCount+ " hotspotInteraction Count "+hsCount)
        logger.info("Total Count: " + (mcCount + extCount+matchCount+icCount+ocCount+htCount+teCount+mlCount+hsCount))
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
