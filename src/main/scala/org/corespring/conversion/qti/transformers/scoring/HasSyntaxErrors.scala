package org.corespring.conversion.qti.transformers.scoring

import org.mozilla.javascript.Context

import scalaz._

object HasSyntaxErrors {
  import org.mozilla.javascript.{ CompilerEnvirons, Parser }

  def apply(js: String): Validation[CustomTransformException, String] = {
    try {
      val cx: Context = Context.enter()
      cx.setLanguageVersion(Context.VERSION_1_5)
      val compilerEnv: CompilerEnvirons = new CompilerEnvirons()
      compilerEnv.initFromContext(cx)
      val parser = new Parser(compilerEnv)
      parser.parse(js, "?", 0)
      Success(js)
    } catch {
      case e: Throwable => {
        println(js)
        Failure(new CustomTransformException("Error parsing js", e))
      }
    } finally {
      /** We must exit the context otherwise we risk deadlock */
      Context.exit()
    }
  }
}

