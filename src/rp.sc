import scala.xml.Node
//val x = scala.xml.XML.load("/664255/664255.xml")

//println(x)

val qti = <assessmentItem>

    <responseProcessing>
      <responseCondition>
        <responseIf>
          <match>
            <variable identifier="RESPONSE1" />
            <correct identifier="RESPONSE1" />
          </match>
          <setOutcomeValue identifier="NUMCORRECT">
            <sum>
              <variable identifier="NUMCORRECT" />
              <baseValue baseType="float">1</baseValue>
            </sum>
          </setOutcomeValue>
        </responseIf>
      </responseCondition>
      <responseCondition>
        <responseIf>
          <or>
            <match>
              <variable identifier="RESPONSE21" />
              <baseValue baseType="string"><![CDATA[12000]]></baseValue>
            </match>
            <match>
              <variable identifier="RESPONSE21" />
              <baseValue baseType="string"><![CDATA[12,000]]></baseValue>
            </match>
          </or>
          <setOutcomeValue identifier="NUMCORRECT">
            <sum>
              <variable identifier="NUMCORRECT" />
              <baseValue baseType="float">1</baseValue>
            </sum>
          </setOutcomeValue>
        </responseIf>
      </responseCondition>
      <responseCondition>
        <responseIf>
          <equal>
            <variable identifier="NUMCORRECT" />
            <baseValue baseType="float">2</baseValue>
          </equal>
          <setOutcomeValue identifier="SCORE">
            <baseValue baseType="float">1</baseValue>
          </setOutcomeValue>
        </responseIf>
        <responseElse>
          <setOutcomeValue identifier="SCORE">
            <baseValue baseType="float">0</baseValue>
          </setOutcomeValue>
        </responseElse>
      </responseCondition>
    </responseProcessing>
  </assessmentItem>

def attributeValueEquals(value: String)(node: Node) = {
  node.attributes.exists(_.value.text == value)
}


val z = "rrst"
val ov = qti \ "responseProcessing" \\ "setOutcomeValue"
val filtered = ov.filter(attributeValueEquals("SCORE"))
val bv = filtered.map(n=> (n \ "baseValue").text).map(s => s.toInt)
val out = bv.sorted.lastOption