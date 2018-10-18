val x = <manifest>
  <resources>
  <resource identifer="1"></resource>
    <resource identifer="2"></resource>
    <resource identifer="3"></resource>
  </resources>
</manifest>


(x \ "resources" \\ "resource").find( n => n.attribute("identifier").map(_.mkString("")) == Some("1"))