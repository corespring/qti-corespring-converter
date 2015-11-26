package org.corespring.conversion.qti.transformers.scoring

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import play.api.libs.json.{JsObject, Json}

class CustomScoringTransformerTest extends Specification {

  import Json._


  def label(t:String) = s"create an executable commonjs module from qti js for componentType: $t"

  "generate" should {

    trait generate extends Scope{
      def comps : Map[String,JsObject]
      lazy val typeMap = comps.map{ case (key, json) => (key -> (json\"componentType").as[String])}
      def qtiJs:String
      lazy val js = CustomScoringTransformer.generate(qtiJs, comps, typeMap).toOption.get
      lazy val runner = new Runner(js)
    }

    trait withResult extends generate{

      def componentType:String
      def answers : String
      def correctness: String

      lazy val comps = Map("A" -> obj("componentType" -> s"corespring-$componentType"))

      lazy val result = runner.process(
        obj(),
        obj("components" ->
          obj("A" ->
            obj("answers" -> answers))),
        obj("components" ->
          obj("A" ->
            obj("correctness" -> correctness))))

    }

    label("drag-and-drop") in new withResult {

    }

    label("text-entry") in new withResult {

      val qtiJs  = """
        |var score = 0;
        |if(A.value === 'apple') score += .1;
        |
        |var outcome = {
        |  score: score
        |}
      """.stripMargin

      val componentType = "text-entry"
      val answers = "apple"
      val correctness = "correct"
      val expectedPercentage = Some(10)
      (result \ "summary" \ "percentage").asOpt[Int] must_== expectedPercentage
    }


    label("line") in new withResult{
      val componentType = "line"

      override val qtiJs =
        """
          |/**
          | * Qti scoring js expects a var with the same name as the responseIdentifier
          | * to be available and to have the form: { outcome: { isCorrect: true|false}, value: 'the response' }
          | *
          | * The js then populates an object called `outcome` with the score.
          | */
          |var score = 0;
          |if (A.outcome.isCorrect) {score += 0.4};
          |if (A.value.indexOf('x') !== -1 ) {score += 0.1};
          |var outcome = {};
          |outcome.score = score;
          |outcome;
        """.stripMargin
      val answers = "2x+4"
      val correctness = "correct"

      (result \ "summary" \ "percentage").asOpt[Int] must_== Some(50)
    }
  }
}
