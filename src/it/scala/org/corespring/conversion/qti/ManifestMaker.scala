package org.corespring.conversion.qti

import scala.xml.Node


object ManifestMaker {


  def wrap(n: Node) = {
    <manifest>
      <resources>
        {n}
      </resources>
    </manifest>
  }

  def manifest(id: String, itemTypeId: String, parccTwoPointScoring: Boolean, partsCount: Int): Node = {
    wrap(
      <resource identifier={id} href={s"$id.xml"} type="imsqti_item_xmlv2p1">
        <metadata>
          <lom>
            <general>
              <itemTypeId>
                {itemTypeId}
              </itemTypeId>
              <sbacTwoPointScoring/>
              <parccTwoPointScoring>
                {if (parccTwoPointScoring) 1 else 0}
              </parccTwoPointScoring>
            </general>
            <parts>
              {(0 until partsCount).map(i => <part itemTypeId={i.toString}/>)}
            </parts>
          </lom>
        </metadata>
        <file href={s"$id.xml"}/>
      </resource>)
  }

}
