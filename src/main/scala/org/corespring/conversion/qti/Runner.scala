package org.corespring.conversion.qti

import java.io.File
import java.util.zip.ZipFile

import com.keydatasys.conversion.zip.KDSQtiZipConverter
import org.corespring.conversion.qti.manifest.QTIManifest
import org.corespring.conversion.zip.ConversionOpts
import org.houghtonmifflinharcourt.conversion.qti.interactions.{ChoiceInteractionTransformer, ExtendedTextInteractionTransformer}
import org.measuredprogress.conversion.zip.MeasuredProgressQtiZipConverter
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.concurrent.Await._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.xml.XML
import java.io.{File, FileInputStream, FileOutputStream, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import org.corespring.qti.models.interactions.ExtendedTextInteraction

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

        if(runOpts.mode == "single-file"){
          val inPath = s"pie/input/" + runOpts.input;
          val outPath = s"pie/output/" + runOpts.output;
          Paths.get(inPath,runOpts.input)

          val xml = XML.loadFile(inPath)
          if( (xml \\ "choiceInteraction").length > 0 ) {
            jsonResult = ChoiceInteractionTransformer.interactionJs(xml, QTIManifest.EmptyManifest)
            logger.info("pie json for choiceInteraction")
          }
          else if( (xml \\ "extendedTextInteraction").length > 0 ) {
            jsonResult = ExtendedTextInteractionTransformer.interactionJs(xml, QTIManifest.EmptyManifest)
            logger.info("pie json for extentedTextInteraction")
          }
          val outFile = new File(outPath)
          val outDirectory = outFile.getParent();
          if((outDirectory != null) && !Files.exists(Paths.get(outDirectory))){
            Files.createDirectory(Paths.get(outDirectory))
          }
          val json = Json.prettyPrint(jsonResult.head._2);
          val basePath = Paths.get(s"${outFile}")
          Files.write(basePath,json.getBytes(StandardCharsets.UTF_8))
          logger.info("file successfully converted into pie json!")
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

