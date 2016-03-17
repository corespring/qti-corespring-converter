package org.parcconline.conversion.qti.processing

/**
 * An exception which defines
 */
class UnsupportedCustomOperatorException(operator: String, id: String) extends Exception {
  override def getMessage() = s"Item $id contains usupported <customOperator/> with class $operator"
}
