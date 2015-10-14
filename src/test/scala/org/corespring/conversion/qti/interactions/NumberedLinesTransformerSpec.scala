package org.corespring.conversion.qti.interactions

import org.corespring.conversion.qti.transformers.ItemTransformer
import org.specs2.mutable.Specification

class NumberedLinesTransformerSpec extends Specification {

  def qti(lines: Seq[Seq[String]]) =
    <assessmentItem>
      <itemBody>
        {
        lines.map(lineSet => {
          <div class="numbered-lines">
            <p class="some container para">
              { lineSet.map(line => <line>{ line }</line>) }
            </p>
          </div>
        })
        }
      </itemBody>
    </assessmentItem>

  "NumberedLinesTransformer" should {

    val lines = Seq(
      Seq("line1", "line2", "line3"),
      Seq("line4", "line5", "line6"))

    val input = qti(lines)
    val output = NumberedLinesTransformer.transform(input, ItemTransformer.EmptyManifest)

    val interactionResults = lines.zipWithIndex.map {
      case (lines, index) => {
        val identifier = s"numbered_lines_${index + 1}"
        NumberedLinesTransformer.interactionJs(input, ItemTransformer.EmptyManifest).get(identifier)
          .getOrElse(throw new RuntimeException(s"No component called $identifier"))
      }
    }

    "return the correct component type" in {
      interactionResults.map(interactionResult =>
        (interactionResult \ "componentType").as[String] must be equalTo "corespring-numbered-lines")
    }

    "returns correct lines" in {
      interactionResults.zipWithIndex.map {
        case (interactionResult, index) => {
          (interactionResult \ "model" \ "lines").as[Seq[String]] diff lines(index) must beEmpty
        }
      }
    }

    "returns properly indexed <corespring-numbered-lines/> nodes" in {
      lines.zipWithIndex.map {
        case (_, index) => {
          (output \\ "corespring-numbered-lines")
            .find(n => (n \\ "@id").text == s"numbered_lines_${index + 1}") must not beEmpty
        }
      }
    }

    "does not return any divs with class='numbered-lines'" in {
      (output \\ "div").filter(div => (div \ "@class").text.contains("numbered-lines")) must beEmpty
    }

  }

}
