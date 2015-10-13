package com.keydatasys.conversion.qti.processing

case class JsResponseProcessing(vars: Map[String, String], lines: Seq[String], responseVars: Seq[String] = Seq.empty) {
  override def toString = (vars.map { case (name, value) => s"var $name = $value;" } ++ lines).mkString("\n")
}