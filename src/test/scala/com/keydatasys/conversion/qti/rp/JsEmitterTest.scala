package com.keydatasys.conversion.qti.rp

import org.specs2.mutable.Specification

class JsEmitterTest extends Specification {


  "js" should {
    "emit" in {

      val rp = Processing(If(

        Or(
          Match("x", StringValue("1")),
          Match("x", StringValue("2"))
        ), SetOutcomeValue("SCORE", Float("1"))))

      val r = JsEmitter.toJs(rp)
      r === ""
    }
  }
}
