package org.corespring.conversion.qti

import org.specs2.mutable.Specification


class KdsRunnerSpec extends Specification with BaseRunnerUtils {

  "kds profile" should {

    val item = new ItemBuilder().xml()
    val zip = new Builder().addItem("1", item).build()
    val out = zip.getParent.resolve("out.zip")
    RunHelper.run(
      zip.toAbsolutePath.toString,
      out.toAbsolutePath.toString,
      "kds",
      None,
      """{"scoringType": "SBAC"}"""
    )

    val profileJson = loadFirstProfileJson(out).get

    "add scoringType to profile.json" in {
      (profileJson \ "taskInfo" \ "extended" \ "kds" \ "scoringType").as[String] must_== "SBAC"
    }

    "add sourceId to profile.json" in {
      (profileJson \ "taskInfo" \ "extended" \ "kds" \ "sourceId").as[String] must_== "1"
    }
    "add title to profile.json" in {
      (profileJson \ "taskInfo" \ "title").as[String] must_== "1 - SBAC"
    }
    "add description to profile.json" in {
      (profileJson \ "taskInfo" \ "description").as[String] must_== "1 - SBAC"
    }
  }
}


