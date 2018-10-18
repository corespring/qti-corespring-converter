


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

val o = SetOutcomeValue("SCORE", Float("1"))
val oo = SetOutcomeValue("NUMCORRECT", Sum("NUMCORRECT", Float("1")))

case class Processing[T: OutcomeValue](ifs: If[T]*)

def toJs(a: Any): String = {

  a match {
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
         |}""".stripMargin
    }
  }
}


val or = Or(
  Match("Response1", StringValue("-frac{sqrt5}3")),
  Match("Response1", StringValue("foo")),
  And(
    Equals("x.y", StringValue("1")),
    Equals("x.y", StringValue("2"))
  )
)

val iff = If(or, o)
toJs(iff)
val x = "??"

