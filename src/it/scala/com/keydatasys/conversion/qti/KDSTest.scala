package com.keydatasys.conversion.qti

import java.io.File
import java.net.URL

import com.typesafe.config._
import org.corespring.container.js.rhino.score.CustomScoreProcessor
import org.corespring.conversion.qti.ManifestMaker
import org.slf4j.LoggerFactory
import org.specs2.mutable.Specification
import org.specs2.specification.Fragments
import play.api.libs.json.{JsArray, JsObject, JsString, Json}
import play.api.libs.json.Json.obj

import scala.collection.JavaConversions._
import scala.xml.{Elem, Node}

case class TestManifest(val xml: Node) {

  private val resource = (xml \ "resources" \ "resource")
  private lazy val lom = (resource \ "metadata" \ "lom")
  private lazy val general = (lom \ "general")

  def itemTypeId = {
    (general \ "itemTypeId").text.trim
  }

  def parts = (lom \ "parts" \\ "part").length

  def parccTwoPointScoring = (general \ "parccTwoPointScoring").text.trim == "1"
}


class KDSTest extends Specification {

  sequential

  private val logger = LoggerFactory.getLogger(this.getClass)

  private def toJson(o: Config, path: String) = {

    if (o.hasPath(path)) {
      Json.parse(o.getObject(path).render(ConfigRenderOptions.concise)).as[JsObject]
    } else {
      obj()
    }
  }

  private def getBoolean(o: Config, path: String, default: Boolean = false): Boolean = {
    if (o.hasPath(path)) {
      o.getBoolean(path)
    } else {
      default
    }
  }

  private def getInt(o: Config, path: String, default: Int = 0): Int = {
    if (o.hasPath(path)) {
      o.getInt(path)
    } else {
      default
    }
  }

  private def resource(path: String): Option[URL] = {
    val out = this.getClass.getResource(path)
    if (out != null) {
      Some(out)
    } else {
      None
    }
  }


  private def loadManifest(config: Config, id: String, mode: KDSMode.Mode): Node = {

    val manifestPath = s"/$id/${mode.toString.toLowerCase()}.resource.xml"

    resource(manifestPath).map(url => {
      val resourceXml = new File(url.toURI)
      ManifestMaker.wrap(scala.xml.XML.loadFile(resourceXml))
    }).getOrElse {
      logger.warn("Try to use the resource xml instead of creating it")
      val itemTypeId = config.getString("manifest.itemTypeId")
      val parccTwoPointScoring = getBoolean(config, "manifest.parccTwoPointScoring")
      val partsCount = getInt(config, "manifest.parts", 0)
      ManifestMaker.manifest(
        id,
        itemTypeId,
        parccTwoPointScoring,
        partsCount)

    }
  }

  def test(id: String) = {

    val raw = ConfigFactory.parseFile(new File(this.getClass.getResource(s"/$id/responses.conf").toURI))
    val correctComponents = toJson(raw, "correct")

    def scoreIsCorrect(qti: Elem, transformer: KDSQtiTransformer, manifest: TestManifest)(c: Config): Fragments = {

      val expectedScore = c.getDouble("expectedScore").toFloat
      val e = toJson(c, "components")
      val components = correctComponents ++ e
      val conversion = transformer.transform(qti, Map(), manifest.xml)

      def isInOverride(key: String) = e.keys.contains(key)

      val outcomes = components.keys.foldRight(obj())((key, json) => {

        val isCorrect = !isInOverride(key)

        val correctScore = {
          (components \ key \ "answers").asOpt[Seq[String]] match {
            case Some(arr) => arr.size
            case _ => 1
          }
        }

        val correctness = if (isCorrect) "correct" else "incorrect"
        val score = if (isCorrect) correctScore else 0
        json ++ obj(key -> obj(
          "score" -> score,
          "correctness" -> correctness
        ))
      })

      logger.info(s"outcomes: $outcomes")


      val playerComponents = (conversion \ "components")

      val result = CustomScoreProcessor.score(
        conversion,
        obj("components" -> components),
        outcomes)

      val correctCount = correctComponents.keys.size - e.keys.size
      val total = correctComponents.keys.size
      s"$correctCount/$total ${if (e.keys.size > 0) s"(incorrect: ${e.keys.mkString(", ")})" else ""} => score: $expectedScore" ! {
        ((result \ "summary" \ "score").as[Float] === expectedScore)
      }
    }

    val sbacManifest = TestManifest(loadManifest(raw, id, KDSMode.SBAC))
    val parccManifest = TestManifest(loadManifest(raw, id, KDSMode.PARCC))

    s"$id" should {

      s"sbac (parts: ${sbacManifest.parts} itemTypeId: ${sbacManifest.itemTypeId})" should {
        val sbacQti = scala.xml.XML.loadFile(new File(this.getClass.getResource(s"/$id/sbac.xml").toURI))
        val sbacList = raw.getConfigList("sbac")
        val transformer = new KDSQtiTransformer(KDSMode.SBAC)
        val manifestXml = loadManifest(raw, id, KDSMode.SBAC)


        (sbacList).foldLeft[Fragments](Fragments())((res, i) => {
          res.append(scoreIsCorrect(sbacQti, transformer, TestManifest(manifestXml))(i))
        })
      }

      s"parcc (parts: ${parccManifest.parts} itemTypeId: ${parccManifest.itemTypeId} twoPointScoring: ${parccManifest.parccTwoPointScoring})" should {

        val parccQti = scala.xml.XML.loadFile(new File(this.getClass.getResource(s"/$id/parcc.xml").toURI))
        val parccList = raw.getConfigList("parcc")
        val manifestXml = loadManifest(raw, id, KDSMode.PARCC)
        val transformer = new KDSQtiTransformer(KDSMode.PARCC)

        (parccList).foldLeft[Fragments](Fragments())((res, i) => {
          res.append(scoreIsCorrect(parccQti, transformer, TestManifest(manifestXml))(i))
        })
      }
    }
  }

  test("664014")
    test("664255")
    test("660295")
    test("663409")
    test("670361")
    test("661656")
    test("663934")
    test("665182")
    test("670508")
    test("670359")
    test("660832")
    test("660827")
}
