package org.measuredprogress.conversion.qti.interactions

import org.corespring.conversion.qti.transformers.InteractionRuleTransformer
import org.specs2.mutable.Specification

import scala.xml.{NodeSeq, Node}

class RRubricBlockTransformerSpec extends Specification {

  val imageUrl = "../images/boy-in-library@2x_c27bef94a7.png"

  def imageNodeFn(imageUrl: String = imageUrl) = <object data={imageUrl} height="312" type="image/png" width="400"/>

  def rubricBlockFn(use: String = "stimulus", imageNode: Node = imageNodeFn()) =
    <rubricBlock use={use} view="candidate author proctor scorer testConstructor tutor">
      <div>
        <p>Mrs. Green's classroom library has 107 books.</p>
        <p>{imageNode}</p>
        <p>Cole says the number of books equals 1 hundred, 7 tens, and 0 ones. Cole is incorrect.</p>
      </div>
    </rubricBlock>

  def qti(rubricBlock: NodeSeq) =
    <itemBody>
      {rubricBlock}
    </itemBody>

  "transform" should {

    "rubricBlock without 'use' attribute equal to 'stimulus'" should {
      val block = rubricBlockFn(use = "rubric")
      val result = new InteractionRuleTransformer(RubricBlockTransformer).transform(qti(rubricBlock = block))

      "be removed" in {
        (result \\ "rubricBlock") must beEmpty
      }
    }

    "rubricBlock with 'use' attribute equal to 'stimulus'" should {
      val block = rubricBlockFn(use = "stimulus")
      val result = new InteractionRuleTransformer(RubricBlockTransformer).transform(qti(rubricBlock = block))

      "be translated to <div class='stimulus'/>" in {
        (result \\ "div").find(div => (div \\ "@class").text.contains("stimulus")) must not beEmpty
      }

      "<object data='image.png' type='image/png'/>" should {

        val result = new InteractionRuleTransformer(RubricBlockTransformer).transform(qti(rubricBlock = rubricBlockFn(imageNode = imageNodeFn(imageUrl))))

        "be translated into <img src='image.png'/>" in {
          (result \\ "img") must not beEmpty;
          (result \\ "img" \ "@src").text must be equalTo(imageUrl.split("/").last)
        }

      }
    }

  }

}
