package org.corespring.conversion.qti

import java.io.File
import java.util.zip.ZipFile

import com.keydatasys.conversion.zip.KDSQtiZipConverter
import org.corespring.conversion.zip.ConversionOpts
import org.measuredprogress.conversion.zip.{MeasuredProgressQtiZipConverter }
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.concurrent.Await._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

case class RunOpts(
                    input: String,
                    output: String,
                    mode: String
                  )


object Run extends App {

    val version = {

    }

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
          // convert a single qti file here ....
          logger.info("convert the file to pie json here ... this is just to get things started .. ")
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

