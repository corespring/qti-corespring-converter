package com.scalapeno.rhinos

import scala.util.control.Exception._
import org.mozilla.javascript._
import play.api.libs.json._

trait RhinosEvaluationSupport { self: RhinosJsonSupport =>
  val scope: RhinosScope

  def eval(javascriptCode: String): Option[JsValue] = {
    withContext[Any] { context =>
      context.evaluateString(scope, javascriptCode, "RhinoContext.eval(String)", 1, null) match {
        case obj: Object if obj == Context.getUndefinedValue() => None
        case obj: Object => Some(obj)
        case _ => None
      }
    }.flatMap(value =>
      value match {
        case Some(value) => Some(toJsValue(value))
        case _ => None
      })
  }

  private[rhinos] def using[X <: {def close()}, A](resource : X)(f : X => A) = {
    try {
      f(resource)
    } finally {
      ignoring(classOf[Exception]) {
        resource.close()
      }
    }
  }
}