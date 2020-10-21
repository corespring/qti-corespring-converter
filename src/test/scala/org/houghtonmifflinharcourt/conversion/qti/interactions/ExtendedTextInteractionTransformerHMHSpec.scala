package org.houghtonmifflinharcourt.conversion.qti.interactions

import org.corespring.conversion.qti.manifest.QTIManifest
import org.specs2.mutable.Specification
import play.api.libs.json.Reads._
import play.api.libs.json._

class ExtendedTextInteractionTransformerHMHSpec extends Specification {
  val identifier = "Q_01"
  val expectedLength = 200
  val expectedLines = 5
  val maxStrings = 20
  val minStrings = 10
  val prompt ="This is prompt text of the CR ite type"

    def qti =
      <assessmentItem>
        <responseDeclaration identifier={ identifier } cardinality="single" baseType="string"/>
        <itemBody>
          <extendedTextInteraction responseIdentifier={ identifier } expectedLength={ expectedLength.toString } expectedLines={ expectedLines.toString } maxStrings={ maxStrings.toString } minStrings={ minStrings.toString }>
          <prompt>{prompt}</prompt>
          </extendedTextInteraction>
        </itemBody>
      </assessmentItem>

    val componentsJson = ExtendedTextInteractionTransformer.interactionJs(qti, QTIManifest.EmptyManifest)
  "transform prompt in Extended Text Interaction" in {
    val json = componentsJson.values.headOption.getOrElse(throw new Exception("There was no result"))
    val prompt = (json \\ "prompt")(0).as[String]
    prompt must be equalTo (prompt)
  }
}