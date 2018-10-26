package com.keydatasys.conversion.qti


case class CustomScoringOpts( mode: KDSMode.Mode, qti: String, resource: String, fileOut: Option[String])

object CustomScoringKds {

  private val parser = new scopt.OptionParser[CustomScoringOpts]("convert-single") {
    head("run", "version")
    opt[String]('m', "mode").action((m, c) => c.copy(mode = KDSMode.withName(m))).required()
    opt[String]('q', "qti").action(( q, c) => c.copy(qti = q)).required()
    opt[String]('r', "resource").action((r, c) => c.copy(resource = r)).required()
    opt[String]('o', "fileOut").action((f, c) => c.copy(fileOut = Some(f)))
  }

  def run(args: Array[String]) = {
    println("run..")
    parser.parse(args, CustomScoringOpts(KDSMode.PARCC, "qti.xml", "resource.xml", None)).map(opts => {

      val transformer = new KDSQtiTransformer(opts.mode)

      val qti = scala.xml.XML.loadFile(opts.qti)
      val resource = scala.xml.XML.loadFile(opts.resource)
      val conversion = transformer.transform(qti, Map(), resource)

      val customScoring = (conversion \ "customScoring").as[String]
      println(customScoring)
    })

  }
}
