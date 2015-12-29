package org.corespring.conversion.qti.transformers.scoring

import play.api.libs.json.JsObject

import scalaz._


case class CustomTransformException(msg: String, t: Throwable = null) extends RuntimeException(msg, t)

object CustomScoringTransformer {

  def generate(qtiJs: String, session: Map[String, JsObject], typeMap: Map[String, String]): Validation[CustomTransformException, String] = synchronized {
    HasSyntaxErrors(qtiJs) match {
      case Failure(e) => Failure(e)
      case Success(js) => Success(wrapJs(js, session, typeMap))
    }
  }


  private def toLocalVar(key: String, componentType: String): String = {
    s"""var $key = toResponseProcessingModel('$key', session.components.$key, '$componentType', outcomes.$key || {});"""
  }

  private def wrapJs(js: String, session: Map[String, JsObject], typeMap: Map[String, String]): String = {

    def getType(key: String) = typeMap.getOrElse(key, "unknown-type")

    s"""
/**
 * CustomScoringTransformer
 * This is a generated js file that wraps qti js into a commonjs module which is v2 compatible format.
 * The module exposes the v2 function: `process(item, session, outcome)`.
 */
var mkValue = function(defaultValue){

  return function(comp, outcome){
    return {
      value: comp && comp.answers ? comp.answers : defaultValue,
      outcome: {
        isCorrect: outcome.correctness === 'correct'
      }
    };
  }
};

var unknownTypeValue = function(comp, outcome){
  return {
    value: '?',
    outcome: {
      isCorrect: false
    }
  }
};

var componentTypeFunctions = {
 'corespring-extended-text-entry' : mkValue('?'),
 'corespring-drag-and-drop' : mkValue([]),
 'corespring-feedback-block' : function(){ return {}; },
 'corespring-focus-task' : mkValue([]),
 'corespring-function-entry' : mkValue('?'),
 'corespring-inline-choice' : mkValue('?'),
 'corespring-line' : mkValue('?'),
 'corespring-multiple-choice' : mkValue([]),
 'corespring-ordering' : mkValue([]),
 'corespring-placement-ordering' : mkValue([]),
 'corespring-point-intercept' : mkValue(['0,0', '0,0']),
 'corespring-select-text' : mkValue([]),
 'corespring-text-entry' : mkValue('?'),
 //If all else fails use this:
 'unknown-type' : unknownTypeValue
};

function toResponseProcessingModel(key, answer, componentType, outcome){
  var fn = componentTypeFunctions[componentType];

  if(!fn){
    throw new Error(key + ' - Can\\\'t find mapping function for ' + componentType);
  }
  return fn(answer, outcome);
}

function pp(o) { return JSON.stringify(o, null, '  '); }

exports.process = function(item, session, outcomes){

  console.log('function=process, item:', pp(item), 'session: ', pp(session), 'outcomes: ', pp(outcomes));

  outcomes = outcomes || {};

  if(!session || !session.components){
    console.log("Error: session has no components: " + JSON.stringify(session));
    return "";
  }

  ${session.map(t => toLocalVar(t._1, getType(t._1))).mkString("\n")}
  ${session.map(t => s"console.log( '${t._1} >', JSON.stringify(${t._1}, null, '  ') ); ").mkString("\n")}

  try{
  /// ----------- this is qti js - can't edit
    $js
  /// -------------- end qti js
  } catch(e){
    return {
      components: {},
      summary: {
        percentage: 0,
        note: 'Error occurred processing: ' + e
      }
    };
  }

  if(Math.floor(outcome.score * 100) > 100){
    console.log("Error: outcome is > 100% - setting it to 100");
  }

  return {
    components: {},
    summary: {
      percentage: Math.min(100, Math.floor(outcome.score * 100)),
      note: 'Overridden score'
    }
  };
};
"""
  }

}
