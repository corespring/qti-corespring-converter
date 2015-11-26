package org.corespring.conversion.qti.transformers.scoring

import org.specs2.mutable.Specification
import org.specs2.specification.{Fragment, Scope}
import play.api.libs.json._

/**
 * Qti scoring js expects a var with the same name as the responseIdentifier
 * to be available and to have the form: { outcome: { isCorrect: true|false}, value: 'the response' }
 *
 * The js then populates an object called `outcome` with the score.
 */
class CustomScoringTransformerTest extends Specification {

  import Json._

  "generate" should {

    def getRunner(js:String,comps:Map[String,JsObject]) = {
      lazy val typeMap = comps.map{ case (key, json) => (key -> (json\"componentType").as[String])}
      lazy val generated = CustomScoringTransformer.generate(js, comps, typeMap).toOption.get
      new Runner(generated)
    }

    def getResult(runner:Runner, isCorrect:Boolean, answers:JsValue, componentType:String) = {

      val correctness = if(isCorrect) "correct" else "incorrect"
      runner.process(
        obj(),
        obj("components" ->
          obj("A" ->
            obj("answers" -> answers))),
        obj("components" ->
          obj("A" ->
            obj("correctness" -> correctness))))

    }

    def componentWithJs(js:String)(ct:String, isCorrect:Boolean, answers:JsValue, expected:Option[Int]) = {
      val comps = Map("A" -> obj("componentType" -> s"corespring-$ct"))
      val p = expected.map(p => s"$p%").getOrElse("nil%")
      s"return $p for ${ct} with answers $answers and isCorrect: $isCorrect" in {
        val runner = getRunner(js, comps)
        val result = getResult(runner,isCorrect, answers, ct)
        (result \ "summary" \ "percentage").asOpt[Int] must_== expected
      }
    }

    val stringBasedComponent = componentWithJs{
      """
        |var score = 0;
        |if(A.value === 'apple') score += 0.5;
        |if(A.outcome.isCorrect) score += 0.5;
        |var outcome = { score: score };
      """.stripMargin
    } _

    val arrayBasedComponent = componentWithJs{
      """
        |var score = 0;
        |if(A.value[0] === 'apple') score += .25;
        |if(A.value[1] === 'banana') score += .25;
        |if(A.outcome.isCorrect) score += 0.5;
        |var outcome = { score: score };
      """.stripMargin
    } _


    "create an executable commonjs module from qti js for componentType and" should {

      Seq(
        "focus-task",
        "select-text",
        "drag-and-drop",
        "focus-task",
        "multiple-choice",
        "placement-ordering").map{
        t =>
          arrayBasedComponent(t, true, arr("apple", "banana"), Some(100))
          arrayBasedComponent(t, false, arr("apple", "banana"), Some(50))
          arrayBasedComponent(t, true, arr("apple"), Some(75))
      }

      Seq(
        "line",
        "text-entry",
        "function-entry",
        "inline-choice",
        "extended-text-entry"
      ).map{ t =>
        stringBasedComponent(t, true, JsString("apple"), Some(100))
      }
      "" in ok
    }
  }
}
