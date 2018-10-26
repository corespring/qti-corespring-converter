package org.corespring.conversion.qti

import java.io.File

import play.api.libs.json.Json
import play.api.libs.json.Json.{obj}
import org.corespring.container.js.rhino.score.CustomScoreProcessor

case class ScoreOpts(js: String, response: String, outcomes: String)

object GetScore {

  private val parser = new scopt.OptionParser[ScoreOpts]("get-score") {
    head("run", "version")
    opt[String]('j', "js").action((js, c) => c.copy(js)).required()
    opt[String]('r', "response").action((r, c) => c.copy(response = r)).required()
    opt[String]('o', "outcomes").action((o, c) => c.copy(outcomes = o)).required()
  }

  private def json(path: String) = {
    val jsonString = scala.io.Source.fromFile(new File(path)).getLines.mkString("\n")
    Json.parse(jsonString)
  }

  def run(args: Array[String]) = {

    parser.parse(args, ScoreOpts(".js", ".response", ".outcomes")).map(o => {
      val js = scala.io.Source.fromFile(new File(o.js)).getLines.mkString("\n")
      val response = json(o.response)
      val outcomes = json(o.outcomes)
      val item = obj("customScoring" -> js)
      val result = CustomScoreProcessor.score(item, response, outcomes)
      println(Json.prettyPrint(result))
    })

  }
}
