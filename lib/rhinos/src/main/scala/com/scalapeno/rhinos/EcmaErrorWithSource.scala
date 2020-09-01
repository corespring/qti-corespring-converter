package com.scalapeno.rhinos

import org.mozilla.javascript.EcmaError

case class EcmaErrorWithSource(ecmaError: EcmaError, source: String) extends Exception {

  override def getMessage = ecmaError.getMessage

}
