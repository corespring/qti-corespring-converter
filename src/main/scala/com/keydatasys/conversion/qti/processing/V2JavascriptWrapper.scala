package com.keydatasys.conversion.qti.processing

object V2JavascriptWrapper {

  import VariableSanitizer._

  /**
    * @param js
    * @param denominator - a denominator for normalization
    * @return
    */
  def wrap(js: JsResponseProcessing, denominator : Option[Int]): String = {

    s"""exports.process = function(item, session, outcomes) {
       |  var answers = session.components;
       |  console.log(">>>> answers:", JSON.stringify(answers))
       |  console.log(">>>> outcomes:", JSON.stringify(outcomes))
       |  function isCorrect(key) {
       |
       |
       |    var out = (outcomes && outcomes[key]) ? (outcomes[key].correctness === 'correct' || outcomes[key].correctness === 'all_correct') : false;
       |    console.log('[isCorrect] ', key, out, outcomes[key]);
       |    return out;
       |  }
       |
       |  function contains(a1, a2) {
       |    for (var i in a2) {
       |      if (a1.indexOf(a2[i]) < 0) {
       |        return false;
       |      }
       |    }
       |    return true;
       |  }
       |
       |  function mapResponse(key) {
       |
       |    var o = (outcomes && outcomes[key]) || {};
       |
       |    if(o.legacyScore){
       |      return o.legacyScore;
       |    } else if(o.correctNum) {
       |       return o.correctNum;
       |    } else {
       |      return o.score || 0;
       |    }
       |  }
       |
       |  ${js.responseVars.map(responseVar => s"var ${responseVar.toVar} = answers['$responseVar'].answers;").mkString("\n|  ")}
       |
       |  ${js.vars.map { case (name, value) => s"var ${name.toVar} = $value;" }.mkString("\n|  ")}
       |
       |  ${js.lines.mkString("\n|  ")}
       |
       |  var normalizedScore = ${denominator.map(d => s"SCORE / $d").getOrElse("SCORE")};
       |  var maxPoints = ${js.responseVars.length};
       |
       |  console.log("SCORE: ", SCORE, "divider: ${denominator.getOrElse("1")}");
       |
       |  var summary = {
       |    ${js.vars.keySet.map(name => s"'${name.toLowerCase}': ${name.toVar}").mkString(",\n|      ")}
       |  };
       |
       |  summary.maxPoints = maxPoints;
       |  summary.__corespringInternal = {
       |    divider : ${denominator.getOrElse(-1)},
       |    responseCount : ${js.responseVars.length}
       |  };
       |  summary.points = SCORE;
       |  summary.score = normalizedScore;
       |  summary.percentage = (normalizedScore * 100).toFixed();
       |
       |  return {
       |    summary: summary
       |  };
       |};""".stripMargin
  }

}
