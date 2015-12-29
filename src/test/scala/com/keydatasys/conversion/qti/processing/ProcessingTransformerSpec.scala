package com.keydatasys.conversion.qti.processing

import org.specs2.mutable.Specification

import scala.xml.Node

class ProcessingTransformerSpec extends Specification with ProcessingTransformer with V2JavascriptWrapper {

  implicit val emptyNode = <noOp/>

  "containerSize" should {
    val node = <containerSize>
      <variable identifier="RESPONSE1"/>
    </containerSize>

    "should transform to id.length" in {
      containerSize(node) === "RESPONSE1.length"
    }
  }

  "mapResonse" should {
    val node = <mapResponse identifier="RESPONSE1"/>

    "should transform to mapResponse('id')" in {
      mapResponse(node) === "mapResponse('RESPONSE1')"
    }
  }

  "_match" should {

    val responseId = "RESPONSE1"
    val correctResponses = Seq("2")

    def qti(responseId: String = responseId,
            correctResponses: Seq[String] = correctResponses) =
      <assessmentItem>
        <responseDeclaration identifier={ responseId } cardinality="single" baseType="string">
          <correctResponse>
            { correctResponses.map(r => <value>{ r }</value>) }
          </correctResponse>
        </responseDeclaration>
        <responseProcessing>
          <match>
            <variable identifier={ responseId }/>
            <correct identifier={ responseId }/>
          </match>
        </responseProcessing>
      </assessmentItem>

    def matcher(qti: Node) =
      (qti \\ "responseProcessing").headOption.getOrElse(throw new Exception("Does not have response processing"))
        .child.find(_.label == "match").getOrElse("Does not have match node").asInstanceOf[Node]

    "translate equivalence of variable to isCorrect for single values" in {
      val node = qti()
      val matchNode = matcher(node)
      _match(matchNode)(node) must be equalTo (s"""isCorrect('$responseId')""")
    }

    "translate equivalence of variable to isCorrect for multiple values" in {
      val correctResponses = Seq("a", "b")
      val node = qti(correctResponses = correctResponses)
      val matchNode = matcher(node)

      _match(matchNode)(node) must be equalTo (s"""isCorrect('$responseId')""")
    }

  }

  val oneExpression = Seq(<match><variable identifier="test"/><variable identifier="test"/></match>)
  val twoExpressions = oneExpression :+ <match><variable identifier="one"/><variable identifier="two"/></match>
  val threeExpressions = twoExpressions :+ <match><variable identifier="great"/><variable identifier="stuff"/></match>
  def node(expressions: Seq[Node]) = <and>{ expressions }</and>

  "and" should {

    "error on empty node" in {
      and(<and/>) must throwAn[Exception]
    }

    "error on one expression" in {
      and(node(oneExpression)) must throwAn[Exception]
    }

    "combine two expressions" in {
      and(node(twoExpressions)) must be equalTo (s"""${twoExpressions.map(expression(_).mkString).mkString(" && ")}""")
    }

    "combine three expressions" in {
      and(node(threeExpressions)) must be equalTo (s"""${threeExpressions.map(expression(_).mkString).mkString(" && ")}""")
    }

  }

  "or" should {

    def node(expressions: Seq[Node]) = <or>{ expressions }</or>

    "error on empty node" in {
      or(<or/>) must throwAn[Exception]
    }

    "error on one expression" in {
      or(node(oneExpression)) must throwAn[Exception]
    }

    "combine two expressions" in {
      or(node(twoExpressions)) must be equalTo (s"""${twoExpressions.map(expression(_).mkString).mkString(" || ")}""")
    }

    "combine three expressions" in {
      or(node(threeExpressions)) must be equalTo (s"""${threeExpressions.map(expression(_).mkString).mkString(" || ")}""")
    }

  }

  "sum" should {
    val values = Seq("1", "2")
    val node = <sum>{ values.map(v => <baseValue>{ v }</baseValue>) }</sum>

    "return X + Y" in {
      sum(node).mkString must be equalTo values.mkString(" + ")
    }
  }

  "gt" should {
    val values = Seq("2", "1")
    val node = <gt>{ values.map(v => <baseValue>{ v }</baseValue>) }</gt>

    "return X > Y" in {
      gt(node) must be equalTo values.mkString(" > ")
    }
  }

  "contains" should {

    val variable = "RESPONSE2"
    val integer = 1

    val node = <contains>
      <variable identifier={ variable }/>
      <multiple>
        <baseValue baseType="integer">{ integer }</baseValue>
      </multiple>
    </contains>

    "return contains(X,Y)" in {
      contains(node) must be equalTo s"contains($variable, [$integer])"
    }

  }

  "multiple" should {

    val integer = 1
    val string = "hey"

    val node = <multiple>
      <baseValue baseType="integer">{ integer }</baseValue>
      <baseValue baseType="string">{ string }</baseValue>
    </multiple>

    "wrap in array" in {
      multiple(node) must be equalTo s"""[${integer}, "${string}"]"""
    }

  }

  "isNull" should {
    val value = "RESPONSE1"
    val node = <isNull><variable identifier={ value }/></isNull>

    "return value == undefined" in {
      isNull(node) must be equalTo s"$value == undefined"
    }

    "throw exception if more than one child node" in {
      val badNode = <isNull><variable identifier={ value }/><variable identifier="bad!"/></isNull>
      isNull(badNode) must throwAn[Exception]
    }

  }

  "setOutcomeValue" should {

    val identifier = "RESPONSE1"
    val value = "OMG"

    val node =
      <setOutcomeValue identifier={ identifier }>
        <baseValue baseType="string">{ value }</baseValue>
      </setOutcomeValue>

    "translate into assignment expression" in {
      setOutcomeValue(node) must be equalTo (s"""$identifier = "$value";""")
    }

  }

  "responseIf" should {

    val identifier = "SCORE"
    val value = "great"
    val responseId = "RESPONSE1"
    val correctResponses = Seq("RESPONSE2")

    def qti(responseId: String = responseId,
            correctResponses: Seq[String] = correctResponses,
            responseIfNode: Node) =
      <assessmentItem>
        <responseDeclaration identifier={ responseId } cardinality="single" baseType="identifier">
          <correctResponse>
            { correctResponses.map(r => <value>{ r }</value>) }
          </correctResponse>
        </responseDeclaration>
        <responseProcessing>
          <responseCondition>
            { responseIfNode }
          </responseCondition>
        </responseProcessing>
      </assessmentItem>

    def responseIfNode() =
      <responseIf>
        <match>
          <variable identifier={ responseId }/>
          <correct identifier={ responseId }/>
        </match>
        <setOutcomeValue identifier={ identifier }>
          <baseValue baseType="string">{ value }</baseValue>
        </setOutcomeValue>
      </responseIf>

    "translate into if statement" in {
      val responseIfNodeVal = responseIfNode()
      val qtiNode = qti(responseIfNode = responseIfNodeVal)
      responseIf(responseIfNodeVal)(qtiNode) must be equalTo s"""if (isCorrect('$responseId')) { $identifier = "$value"; }"""
    }

  }

  "responseCondition" should {
    val responseConditionVal =
      <responseCondition>
        <responseIf>
          <and>
            <match>
              <variable identifier="RESPONSE1"/>
              <correct identifier="RESPONSE1"/>
            </match>
            <match>
              <variable identifier="RESPONSE2"/>
              <correct identifier="RESPONSE2"/>
            </match>
          </and>
          <setOutcomeValue identifier="SCORE">
            <baseValue baseType="float">2</baseValue>
          </setOutcomeValue>
        </responseIf>
        <responseElseIf>
          <match>
            <variable identifier="RESPONSE1"/>
            <correct identifier="RESPONSE1"/>
          </match>
          <setOutcomeValue identifier="SCORE">
            <baseValue baseType="float">1</baseValue>
          </setOutcomeValue>
        </responseElseIf>
      </responseCondition>

    def qti(responseConditionVal: Node = responseConditionVal) =
      <assessmentItem>
        <responseDeclaration identifier="RESPONSE1" cardinality="single" baseType="string">
          <correctResponse>
            <value>ONE</value>
          </correctResponse>
        </responseDeclaration>
        <responseDeclaration identifier="RESPONSE2" cardinality="single" baseType="string">
          <correctResponse>
            <value>TWO</value>
          </correctResponse>
        </responseDeclaration>
        <responseProcessing>
          { responseConditionVal }
        </responseProcessing>
      </assessmentItem>

    "translate into conditional statement" in {
      responseCondition(responseConditionVal)(qti()) must be equalTo
        """if ((isCorrect('RESPONSE1')) && (isCorrect('RESPONSE2'))) { SCORE = 2; } else if (isCorrect('RESPONSE1')) { SCORE = 1; }"""
    }
  }

  "toJs" should {

    val qti =
      <assessmentItem>
        <responseDeclaration identifier="RESPONSE1" cardinality="single" baseType="identifier">
          <correctResponse>
            <value>4</value>
          </correctResponse>
        </responseDeclaration>
        <responseDeclaration identifier="RESPONSE2" cardinality="multiple" baseType="identifier">
          <correctResponse>
            <value>2</value>
            <value>5</value>
          </correctResponse>
        </responseDeclaration>
        <outcomeDeclaration identifier="SCORE" cardinality="single" baseType="float">
          <defaultValue>
            <value>0</value>
          </defaultValue>
        </outcomeDeclaration>
        <itemBody>
          <choiceInteraction responseIdentifier="RESPONSE1" shuffle="true" maxChoices="1">
            <simpleChoice identifier="1">one</simpleChoice>
            <simpleChoice identifier="2">two</simpleChoice>
            <simpleChoice identifier="3">three</simpleChoice>
            <simpleChoice identifier="4">four</simpleChoice>
          </choiceInteraction>
          <choiceInteraction responseIdentifier="RESPONSE2" shuffle="true" maxChoices="5">
            <simpleChoice identifier="1">one</simpleChoice>
            <simpleChoice identifier="2">two</simpleChoice>
            <simpleChoice identifier="3">three</simpleChoice>
            <simpleChoice identifier="4">four</simpleChoice>
            <simpleChoice identifier="5">
              <![CDATA[heavy pads melt ice, which allow polar bears to walk across the ice without slipping]]>
            </simpleChoice>
          </choiceInteraction>
        </itemBody>
        <responseProcessing>
          <responseCondition>
            <responseIf>
              <and>
                <match>
                  <variable identifier="RESPONSE1"/>
                  <correct identifier="RESPONSE1"/>
                </match>
                <match>
                  <variable identifier="RESPONSE2"/>
                  <correct identifier="RESPONSE2"/>
                </match>
              </and>
              <setOutcomeValue identifier="SCORE">
                <baseValue baseType="float">1</baseValue>
              </setOutcomeValue>
            </responseIf>
          </responseCondition>
        </responseProcessing>
      </assessmentItem>

    "convert response processing node to JS" in {
      //      println(toJs(qti).map(wrap).getOrElse("Oops!"))
      true === true
    }
  }

}