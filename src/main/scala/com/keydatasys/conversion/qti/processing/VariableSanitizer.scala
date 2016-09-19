package com.keydatasys.conversion.qti.processing

object VariableSanitizer {

  implicit class StringToVar(string: String) {
    def toVar = string.replaceAll("-", "_")
  }

}
