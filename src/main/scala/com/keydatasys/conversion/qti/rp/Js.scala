package com.keydatasys.conversion.qti.rp

object JsEmitter {

  def explainFailure(c: IfCondition) :String = {
    c match {
      case And(c@_*) => c.map(explainFailure).mkString("\n")
      case Or(c@_*) => c.map(explainFailure).mkString("\n")
      case Equals(id, v) => s"console.log( '$id === ${toJs(v)}? ', $id === ${toJs(v)} )"
      case Match(id, v) => s"console.log( '$id === ${toJs(v)}? ', $id === ${toJs(v)} )"
    }
  }
  def toJs(a: Any): String = {

    a match {
      case Processing(ifs@_*) => ifs.map(toJs).mkString("\n")
      case SetOutcomeValue(id, v) => s"$id = ${toJs(v)}"
      case Float(v) => v.toFloat.toString
      case Sum(id, v) => s" ${id} + ${toJs(v)}"
      case Match(id, v) => s"$id === ${toJs(v)}"
      case Equals(id, v) => s"$id === ${toJs(v)}"
      case StringValue(v) => s"'$v'"
      case Or(matches@_*) => s"(${matches.map(toJs).mkString(" || ")})"
      case And(matches@_*) => s"(${matches.map(toJs).mkString(" && ")})"
      case If(c, t) => {
        s"""if(${toJs(c)})
           |{
           |  ${toJs(t)}
           |} else {
           |  ${explainFailure(c)}
           |}""".stripMargin
      }
    }
  }

}
