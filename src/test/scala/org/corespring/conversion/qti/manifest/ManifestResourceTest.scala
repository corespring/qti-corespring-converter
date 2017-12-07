package org.corespring.conversion.qti.manifest

import org.specs2.mutable.Specification

import scala.xml.Node

class ManifestResourceTest extends Specification {

  val xml : Node = <root>

    <img src="img-one"></img>
    <img src="img-two"></img>
    <img></img>
    <video>
      <source src="vid-one"></source>
      <source src="vid-one-a"></source>
    </video>
    <video>
      <source src="vid-two"></source>
    </video>
    <video>
      <source></source>
    </video>

    <audio>
      <source src="audio-one"></source>
      <source src="audio-one-a"></source>
    </audio>
    <audio>
      <source src="audio-two"></source>
    </audio>
    <audio>
      <source></source>
    </audio>

    <stylesheet href="stylesheet-one"></stylesheet>
    <stylesheet href="stylesheet-two"></stylesheet>
    <stylesheet ></stylesheet>
  </root>

  val extracted = ManifestResource.extractSources(xml)

  "extractSources" should {
    "extract img" in {
      extracted.get(ManifestResourceType.Image) must_== Some(Seq("img-one", "img-two"))
    }

    "extract video" in {
      extracted.get(ManifestResourceType.Video) must_== Some(Seq("vid-one", "vid-one-a", "vid-two"))
    }
    "extract audio" in {
      extracted.get(ManifestResourceType.Audio) must_== Some(Seq("audio-one", "audio-one-a", "audio-two"))
    }
    "extract stylesheet" in {
      extracted.get(ManifestResourceType.StyleSheet) must_== Some(Seq("stylesheet-one", "stylesheet-two"))
    }
  }
}
