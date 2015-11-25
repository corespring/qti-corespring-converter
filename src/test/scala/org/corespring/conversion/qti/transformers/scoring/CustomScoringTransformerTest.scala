package org.corespring.conversion.qti.transformers.scoring

import org.specs2.mutable.Specification
import play.api.libs.json.Json

class CustomScoringTransformerTest extends Specification {

  import Json._

  "generate" should {

    "create executable js for x" in {
      val js = CustomScoringTransformer.generate("alert('hi');", Map.empty, Map.empty).toOption.get
      val runner = new Runner(js)
      val result = runner.process(obj(), obj(), obj())

      println(result)
      (result \ "summary" \ "percentage").asOpt[Int] must_== 0
    }
  }
}



