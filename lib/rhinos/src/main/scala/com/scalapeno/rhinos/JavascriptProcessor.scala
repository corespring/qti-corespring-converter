package com.scalapeno.rhinos

import play.api.libs.json._
import play.api.libs.json.JsValue
import org.mozilla.javascript.EcmaError

trait JavascriptProcessor {

  val identifierRegex = "[a-zA-Z_$][0-9a-zA-Z_$]*".r

  val reservedWords = Seq("break", "case", "catch", "class", "continue", "debugger", "default", "delete", "do", "else",
    "enum", "export", "extends", "finally", "for", "function", "if", "implements", "import", "in", "interface",
    "instanceof", "let", "new", "package", "private", "protected", "public", "return", "static", "super", "switch",
    "this", "throw", "try", "typeof", "var", "void", "while", "with", "yield")

  /**
   * Returns true if an identifier is a valid Javascript identifier, false otherwise.
   *
   * TODO: This is **not** comprehensive. You'll note here: http://stackoverflow.com/a/9337047 that there are many
   * valid unicode characters that aren't alphanumeric. For the time being (maybe forever), we're being strict.
   */
  def validIdentifier(name: String): Boolean = name match {
    case identifierRegex() => !reservedWords.contains(name)
    case _ => false
  }

  def toVar(name: String, value: Any): String = value match {
    case string: String => s"""var $name = "$string";"""
    case int: Int => s"var $name = $int;"
    case long: Long => s"var $name = $long;"
    case double: Double => s"var $name = $double;"
    case jsValue: JsValue => s"var $name = ${Json.toJson(jsValue)};"
    case _ => throw new IllegalArgumentException(
      s"Cannot convert object of type ${value.getClass.toString} to Javascript var")
  }

  def js(javascript: String, variables: Map[String, Any] = Map.empty): Option[(String,JsValue)] = {
    val script = s"${variables.map{ case(k,v) => toVar(k, v) }.mkString("\n")}\n$javascript"
    try {
      new RhinosRuntime().eval(script).map(script -> _)
    } catch {
      case e: EcmaError => throw new EcmaErrorWithSource(e, script)
    }
  }

}