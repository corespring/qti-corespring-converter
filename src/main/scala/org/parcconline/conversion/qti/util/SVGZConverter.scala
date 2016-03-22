package org.parcconline.conversion.qti.util

import java.io._
import java.nio.charset.StandardCharsets
import java.util.zip.GZIPInputStream

import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.batik.bridge._
import org.apache.batik.transcoder._
import org.apache.batik.util.XMLResourceDescriptor
import org.apache.commons.io.FileUtils
import org.corespring.common.file.SourceWrapper

import org.apache.batik.transcoder.image.PNGTranscoder

object SVGZConverter {

  def convert(source: SourceWrapper): SourceWrapper = {
    val result = GzFileIterator(source.getFile, "UTF-8")
    val outFile = File.createTempFile(source.name, source.name.split(".").lastOption.getOrElse("svg"))
    outFile.deleteOnExit
    val transcoder = new PNGTranscoder()

    val input = new TranscoderInput(new ByteArrayInputStream(result.mkString.getBytes(StandardCharsets.UTF_8)))
    val outputStream = new FileOutputStream(outFile)
    val output = new TranscoderOutput(outputStream)

    transcoder.transcode(input, output)
    outputStream.flush()
    outputStream.close()

    SourceWrapper(source.name.replace("svg", "svgz"), FileUtils.openInputStream(outFile))
  }

  def getDimensions(source: SourceWrapper): (Int, Int) = {
    val result = GzFileIterator(source.getFile, "UTF-8")
    val file = File.createTempFile(source.name, source.name.split(".").lastOption.getOrElse("svg"))
    file.deleteOnExit
    val outputStream = new FileOutputStream(file)
    outputStream.write(result.mkString.getBytes(StandardCharsets.UTF_8))
    outputStream.close()

    val is = new FileInputStream(file)
    val factory = new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName())
    val document = factory.createDocument(file.toURI().toURL().toString(), is)
    val agent = new UserAgentAdapter()
    val loader = new DocumentLoader(agent)
    val context = new BridgeContext(agent, loader)
    context.setDynamic(true)
    val builder = new GVTBuilder()
    val root = builder.build(context, document)

    (root.getPrimitiveBounds().getWidth().toInt, root.getPrimitiveBounds().getHeight().toInt)
  }

  class BufferedReaderIterator(reader: BufferedReader) extends Iterator[String] {
    override def hasNext() = reader.ready
    override def next() = reader.readLine()
  }

  object GzFileIterator {
    def apply(file: java.io.File, encoding: String) = {
      new BufferedReaderIterator(
        new BufferedReader(
          new InputStreamReader(
            new GZIPInputStream(
              new FileInputStream(file)), encoding)))
    }
  }

}
