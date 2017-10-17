package org.corespring.conversion.qti

import java.io.File
import java.util.zip.ZipFile

import com.keydatasys.conversion.zip.KDSQtiZipConverter
import com.progresstesting.conversion.zip.ProgressTestingQtiZipConverter
import org.corespring.conversion.zip.ConversionOpts
import org.measuredprogress.conversion.zip.{MeasuredProgressQtiZipConverter, OldMeasuredProgressQtiZipConverter}
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.concurrent.Await._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

case class RunOpts(
                    input: String,
                    output: String,
                    vendor: String,
                    metadata: String = "{}",
                    limit: Int = 0,
                    sourceIds: Seq[String] = Seq.empty,
                    killRuntime: Boolean = true
                  )


object Runner extends App {


  val logger = LoggerFactory.getLogger(Runner.this.getClass)

  val parser = new scopt.OptionParser[RunOpts]("run") {
    head("run", "version")

    opt[Int]('l', "limit").action((l, c) => c.copy(limit = l))

    opt[String]('i', "input").required().action((i, c) => c.copy(input = i))

    opt[String]('o', "output").required().action((o, c) => c.copy(output = o))

    opt[String]('v', "vendor")
      .required().action((v, c) => c.copy(vendor = v))
      .validate(v => {
        val vendors = converters.keys.toSeq
        if (vendors.contains(v)) success else failure("unknown vendor")
      })

    opt[String]('m', "metadata")
      .action((m, c) => c.copy(metadata = m))
      .validate(m => {
        try {
          Json.parse(m).as[JsObject]
          success
        } catch {
          case t: Throwable => failure("not valid json")
        }
      })

    opt[String]('s', "sourceId").action((s, c) => c.copy(sourceIds = c.sourceIds ++ Seq(s)))

    opt[String]('l', "sourceIdList")
      .text("A path to a file with a source id on its own line")
      .action((l, c) => {
      val ids = Source.fromFile(new File(l))
        .getLines()
        .map(_.trim)
        .filterNot(_.isEmpty)
        .toSeq
      c.copy(sourceIds = c.sourceIds ++ ids)
    })

    opt[Boolean]('r', "killRuntime").action((r, c) => c.copy(killRuntime = r))
  }


  val converters = Map(
    "kds" -> KDSQtiZipConverter,
    "progresstesting" -> ProgressTestingQtiZipConverter,
    "measuredprogress" -> MeasuredProgressQtiZipConverter,
    "old-measuredprogress" -> OldMeasuredProgressQtiZipConverter
  )

  parser.parse(args, RunOpts("", "", "")) match {
    case Some(runOpts) => {
      val input = new ZipFile(runOpts.input)
      val outputPath = runOpts.output
      val vendor = runOpts.vendor
      val metadata = Json.parse(runOpts.metadata).as[JsObject]

      val converter = converters
        .get(vendor).getOrElse(throw new IllegalArgumentException(s"You must specify a supported vendor: ${converters.keys.mkString(", ")}"))

      val opts = ConversionOpts(
        runOpts.limit,
        runOpts.sourceIds
      )


      logger.info(s"convertion opts: $opts")

      val outFile = new File(runOpts.output)

      if (outFile.exists()) {
        logger.info(s"Deleting ${runOpts.output}")
        outFile.delete
      }

      result(converter.convert(input, outputPath, Some(metadata), opts)
        .map(_ => {
          println(s"all done sourceIds: ${opts.sourceIds}")

          if (runOpts.killRuntime) {
            Runtime.getRuntime().halt(0)
          }
        }), 25.minutes)
    }
    case None => {
      println(parser.usage)
      sys.exit(1)
    }
  }

}

