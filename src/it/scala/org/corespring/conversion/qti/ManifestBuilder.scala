package org.corespring.conversion.qti

import java.io.File
import java.net.URL
import java.nio.file.{Files, Path, Paths}

import org.corespring.common.xml.XhtmlParser
import org.slf4j.LoggerFactory

import scala.io.Source
import scala.sys.process.Process
import scala.xml._
import scala.xml.transform.{RewriteRule, RuleTransformer}

trait Instruction

case class AddItem(id: String, node: Elem) extends Instruction
case class AddMp3(name: String, src: String) extends Instruction
case class AddCss(contents: String, name: String, addToResource:Boolean) extends Instruction

class Builder(instructions: Seq[Instruction] = Seq.empty) {

  val logger = LoggerFactory.getLogger(this.getClass)

  def addItem(id: String, node: Elem): Builder = {
    new Builder(this.instructions :+ AddItem(id, node))
  }

  def addMp3(name:String, src:String) : Builder = {
    new Builder(this.instructions :+ AddMp3(name, src))
  }

  def addCss(contents:String, name:String, addToResource:Boolean = false) : Builder = {
    new Builder(this.instructions :+ AddCss(contents, name, addToResource))
  }

  private def addResourceNode(i: Instruction): Option[Node] = i match {
    case AddItem(id, _) => {
      val r = <resource
      identifier={id}
      type="imsqti_item_xmlv2p1"
      href={s"$id.xml"}>
        <metadata>
          <lom>
            <general>
              <identifier>
                {id}
              </identifier>
            </general>
          </lom>
        </metadata>
        <file href={s"$id.xml"}/>
      </resource>
      Some(r)
    }
    case _ => None
  }


  private def buildManifest(): Elem = {

    val nodes: Seq[Node] = this.instructions.flatMap { i => addResourceNode(i) }

    <manifest>
      <resources>
        {nodes}
      </resources>
    </manifest>
  }

  def build(): Path = {
    //1. create a tmp dir
    val dir = Files.createTempDirectory("builder")
    //2. write qti (make sure id is in place)
    instructions.foreach {
      case AddItem(id, node) => {
        val update = node.copy(attributes = node.attributes.copy(Attribute(None, "identifier", Text(id), Null)))
        val path = dir.resolve(s"$id.xml")
        scala.xml.XML.save(path.toAbsolutePath.toString, update.asInstanceOf[Node], "UTF-8", true, null)
      }
      case AddMp3(name, src) => {
        val srcPath = Paths.get(this.getClass.getResource(src).toURI)
        val outPath = dir.resolve(name)
        Files.copy(srcPath, outPath)
      }

      case AddCss(contents, name, addToResource) => {
        if(addToResource){
          throw new NotImplementedError("todo")
        }
        val outPath = dir.resolve(name)
        Files.createDirectories(outPath.getParent)
        Files.write(outPath, contents.getBytes("UTF-8"))
      }
      case _ => // do nothing
    }

    //3. add resource to manifest
    val manifest = buildManifest()
    val manifestPath = dir.resolve("imsmanifest.xml")
    scala.xml.XML.save(manifestPath.toAbsolutePath.toString, manifest, "UTF-8", true, null)
    //4. write manifest
    //5. zip it up
    val zipPath = buildZip(dir)
    zipPath.toAbsolutePath
  }

  def buildZip(dir: Path): Path = {
    val out = dir.resolve(s"built.zip")
    val cmd = Seq("zip", "-r", out.toAbsolutePath.toString, ".")
    val code = Process(cmd, new File(dir.toUri)).!
    logger.info(s"buildZip code: $code")
    out
  }
}

object ItemBuilder {
  val defaultQti = <assessmentItem>
    <itemBody>
    </itemBody>
  </assessmentItem>
}

class ItemBuilder(e: Elem = ItemBuilder.defaultQti) {


  private def applyTransform(t:RuleTransformer) : ItemBuilder = {
    new ItemBuilder(t.transform(Seq(this.e.asInstanceOf[Node])).head.asInstanceOf[Elem])
  }

  def addStylesheet(styleNode:Node) : ItemBuilder = {

    val t = new RuleTransformer(new RewriteRule{
      override def transform(n:Node) = n match   {
        case e : Elem if e.label == "assessmentItem" => {
          e.copy(child = e.child ++ styleNode)
        }
        case _ => n
      }
    })
    applyTransform(t)
  }

  /**
    * If you need to add CDATA - send in an xml string for raw parsing.
    * @param s
    * @return
    */
  def addToItemBody(s:String): ItemBuilder = {
    addToItemBody(rawParse(s).asInstanceOf[Elem])
  }

  def addToItemBody(body: Elem): ItemBuilder = {

    val t = new scala.xml.transform.RuleTransformer(
      new RewriteRule {
        override def transform(n: Node) = n match {
          case e: Elem if e.label == "itemBody" => {
            e.copy(child = e.child ++ body)
          }
          case _ => n
        }
      })
    applyTransform(t)
  }

  def rawParse(s:String) : Node = XhtmlParser.loadString(s)


  def addResponseDeclaration(rd: String): ItemBuilder = {
    this.addResponseDeclaration(this.rawParse(rd))
  }

  def addResponseDeclaration(rd: Node) : ItemBuilder = {
    val t = new RuleTransformer(new RewriteRule {
      override def transform(n: Node) = n match {
        case e: Elem if e.label == "assessmentItem" => {
          e.copy(child = rd ++ e.child)
        }
        case _ => n
      }
    })
    new ItemBuilder(t.transform(Seq(this.e.asInstanceOf[Node])).head.asInstanceOf[Elem])
  }

  def xml() = this.e
}

object ManifestUtils {


  def buildItem(s: String) = {

    val content = XhtmlParser.loadString(s)

    <assessmentItem>
      <itemBody>
        {content}
      </itemBody>
    </assessmentItem>
  }
}
