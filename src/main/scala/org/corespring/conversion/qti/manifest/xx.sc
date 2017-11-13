val xml = <root>
  <video>
<source src="foo"/>
  </video>
  <video>
    <source src="bar"/>
  </video>
</root>

(xml \\ "video" ).map( n => (n \ "source" \ "@src" ).text)


(xml \\ "video").map(_ \ "source").map(_ \ "@src").map(_.toString)
