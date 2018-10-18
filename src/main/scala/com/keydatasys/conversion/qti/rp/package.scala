package com.keydatasys.conversion.qti

package object rp {


  case class Processing[T:OutcomeValue] (ifs: If[T]*)

  case class Sum(variable: String, value: Value)

  trait Value {
    def value: String

    def valueType: String
  }

  case class Float(value: String) extends Value {
    def valueType = "float"
  }

  case class StringValue(value: String) extends Value {
    def valueType = "string"
  }

  class OutcomeValue[T]

  object OutcomeValue {

    implicit object FloatWitness extends OutcomeValue[Float]

    implicit object StringWitness extends OutcomeValue[String]

    implicit object SumWitness extends OutcomeValue[Sum]

  }


  trait IfCondition

  case class And(c: IfCondition*) extends IfCondition

  case class Or(c: IfCondition*) extends IfCondition

  case class Equals(id: String, v: Value) extends IfCondition

  case class Match(id: String, v: Value) extends IfCondition

  case class If[T: OutcomeValue](c: IfCondition, thenDo: SetOutcomeValue[T])


  case class SetOutcomeValue[T: OutcomeValue](id: String, v: T)
}
