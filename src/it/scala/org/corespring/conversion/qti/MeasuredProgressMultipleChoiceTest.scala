package org.corespring.conversion.qti


class MeasuredProgressMultipleChoiceTest extends BaseRunner {

  override def vendor = "measuredprogress"

  override def sourceId = "RES-5512cf90-9e58-4b8b-ab35-382e0a238824"

  "measuredprogress " should {

    "add value to corespring-multiple-choice" in {
      playerDef.map(json(zip, _)).map { json =>
        (json \ "components" \ "RESPONSE618" \ "componentType").as[String] must_== "corespring-multiple-choice"
      }.getOrElse(ko)
    }
  }
}
