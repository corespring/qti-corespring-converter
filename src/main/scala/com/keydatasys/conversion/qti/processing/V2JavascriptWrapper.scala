package com.keydatasys.conversion.qti.processing

trait V2JavascriptWrapper {

  import VariableSanitizer._

  def wrap(js: JsResponseProcessing): String = {
    s"""exports.process = function(item, session, outcomes) {
       |  var answers = session.components;
       |
       |  function isCorrect(key) {
       |    return (outcomes && outcomes[key]) ? (outcomes[key].correctness === 'correct' || outcomes[key].correctness === 'all_correct') : false;
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
       |  var divider = ${ if (js.responseVars.isEmpty) 1 else js.responseVars.length}
       |  var normalizedScore = SCORE / divider;
       |  var maxPoints = ${js.responseVars.length};
       |
       |  var summary = {
       |    ${js.vars.keySet.map(name => s"'${name.toLowerCase}': ${name.toVar}").mkString(",\n|      ")}
       |  };
       |
       |  summary.maxPoints = maxPoints;
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
