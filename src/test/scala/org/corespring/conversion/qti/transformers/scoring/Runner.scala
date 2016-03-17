package org.corespring.conversion.qti.transformers.scoring

import org.corespring.rhino.ConsoleScriptable
import org.mozilla.javascript._
import play.api.libs.json.{JsString, Json, JsValue}
import org.mozilla.javascript.{Function => RhinoFunction, Context, Scriptable}


class Runner(js: String) extends JsFunctionCalling {
  val wrapped =
    s"""
       |exports = {}
       |
       |console = {
       |  log: function(){
       |  }
       |}
       |
       |$js
       |
       """.stripMargin

  private def getScoringObject(ctx: Context, scope: Scriptable): Scriptable = {
    scope.get("exports", scope).asInstanceOf[Scriptable]
  }

  def process(item: JsValue, answers: JsValue, computedOutcomes: JsValue): JsValue = {
    implicit val context: Context = Context.enter()
    try {
      implicit val scope: Scriptable = context.initStandardObjects()

      ScriptableObject.defineClass(scope, classOf[ConsoleScriptable])
      val cs : Scriptable = context.newObject(scope, "ConsoleScriptable")
      ScriptableObject.defineProperty(scope, "console", cs, ScriptableObject.READONLY | ScriptableObject.PERMANENT)

      context.evaluateString(scope, wrapped, "EvaluationScript", 1, null)
      val scoringObject = getScoringObject(context, scope)
      val processFn = scoringObject.get("process", scoringObject).asInstanceOf[RhinoFunction]
      callJsFunctionJson(wrapped, processFn, scoringObject, Array(item, answers, computedOutcomes)) match {
        case Left(e) => throw new RuntimeException(e.toString)
        case Right(json) => json
      }
    } catch {
      case e: Throwable => {
        throw new RuntimeException(e)
      }
    } finally {
      Context.exit()
    }
  }
}


trait JsFunctionCalling {

  def jsObject(json: JsValue)(implicit ctx: Context, scope: Scriptable): AnyRef = {
    val jsonString = Json.stringify(json)
    json match {
      case s: JsString => Context.javaToJS(s.value, scope)
      case _ => toObject.call(ctx, scope, scope, Array(jsonString))
    }
  }

  def jsJson(implicit scope: Scriptable) = scope.get("JSON", scope).asInstanceOf[ScriptableObject]

  def toObject(implicit scope: Scriptable): RhinoFunction = jsJson.get("parse", jsJson).asInstanceOf[RhinoFunction]

  def toJsonString(implicit scope: Scriptable): RhinoFunction = {
    jsJson.get("stringify",
      jsJson).asInstanceOf[RhinoFunction]
  }

  def callJsFunctionJson(
                          rawJs: String,
                          fn: RhinoFunction,
                          parentScope: Scriptable,
                          args: Array[JsValue])(implicit ctx: Context, rootScope: Scriptable): Either[EcmaError, JsValue] = {
    def mkJson(o: Any): JsValue = {
      val jsonString: String = NativeJSON.stringify(ctx, rootScope, o, null, null).asInstanceOf[String]
      Json.parse(jsonString.toString)
    }
    callJsFunction(rawJs, fn, parentScope, args, mkJson)(ctx, rootScope)
  }

  def callJsFunctionBoolean(
                             rawJs: String,
                             fn: RhinoFunction,
                             parentScope: Scriptable,
                             args: Array[JsValue])(implicit ctx: Context, rootScope: Scriptable): Either[EcmaError, Boolean] = {
    def mkBoolean(o: Any): Boolean = {
      Context.jsToJava(o, classOf[java.lang.Boolean]).asInstanceOf[Boolean]
    }
    callJsFunction(rawJs, fn, parentScope, args, mkBoolean)(ctx, rootScope)
  }

  def callJsFunction[A](
                         rawJs: String,
                         fn: RhinoFunction,
                         parentScope: Scriptable,
                         args: Array[JsValue],
                         makeResult: Any => A)(implicit ctx: Context, rootScope: Scriptable): Either[EcmaError, A] = {
    try {
      val jsArgs: Array[AnyRef] = args.toArray.map(jsObject(_))
      val result = fn.call(ctx, rootScope, parentScope, jsArgs)
      Right(makeResult(result))
    } catch {
      case e: EcmaError => Left(e)
      case e: Throwable => throw new RuntimeException("General error while processing js", e)
    }
  }
}

