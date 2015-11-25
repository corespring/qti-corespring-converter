package org.corespring.conversion.qti.transformers.scoring

import org.specs2.mutable.Specification
import play.api.libs.json.Json

class CustomScoringTransformerTest extends Specification {

  import Json._

  "generate" should {

    "create executable js for x" in {

      val typeMap = Map("A"->"corespring-line")
      val comps = Map("A" -> obj("componentType" -> "corespring-line"))

      val qtiJs =
        """
          |    var score = 0;
          |    if (A.outcome.isCorrect) {score += 0.4};
          |    if (A.value.indexOf('x') !== -1 ) {score += 0.1};
          |    var correctAnswers = 0;
          |    var outcome = {};
          |    outcome.score = score;
          |    outcome;
        """.stripMargin
      val js = CustomScoringTransformer.generate(qtiJs, comps, typeMap).toOption.get

      js.lines.zipWithIndex.foreach{ case (l, n) => println(s"$n: $l") }

      val runner = new Runner(js); //"console.log('hi there', 'hows it going');")
      val result = runner.process(
          obj(),
          obj("components" ->
            obj("A" ->
              obj("anwers" -> "2x+4"))),
          obj("components" ->
            obj("A" ->
              obj("isCorrect" -> true))))

      (result \ "summary" \ "percentage").asOpt[Int] must_== 0
    }
  }
}



