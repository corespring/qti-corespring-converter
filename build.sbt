import sbt._
import sbt.Keys._
// import org.corespring.sbt.repo.RepoAuthPlugin.Keys._
//import com.typesafe

enablePlugins(JavaAppPackaging)
enablePlugins(BuildInfoPlugin)

resolvers in ThisBuild ++= Seq(
  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
  "plugins" at "https://bintray.com/sbt/sbt-plugin-releases/"
)

fork in Test := true

organization in ThisBuild := "org.pie"
scalaVersion in ThisBuild := "2.13.3"

val sharedDependencies = Seq(
  "org.scalaz" %% "scalaz-core" % "7.3.2",
  "com.typesafe.play" %% "play-json" % "2.9.0",
  "org.specs2" %% "specs2-core" % "4.10.0" % "test"
)

lazy val rhinos = Project("rhinos", base = file("lib/rhinos"))
  .settings(

    scalacOptions in Test ++= Seq("-Yrangepos"),
    libraryDependencies ++= sharedDependencies ++ Seq(
      "org.mozilla" % "rhino" % "1.7.12",
      "com.typesafe.play" %% "play-json" % "2.9.0" % "provided",
      "org.slf4j" %  "slf4j-api" % "1.6.4",
      "ch.qos.logback" % "logback-classic" % "1.0.0" % "provided",
    )
  )
lazy val macros = Project("macros", file("lib/macros"))
  .settings(
    scalacOptions in Test ++= Seq("-Yrangepos"),
    libraryDependencies ++= Seq( "org.scala-lang" % "scala-reflect" % "2.13.3")
  )

lazy val qti = Project("qti-lib", file("lib/qti"))
  .settings(
    scalacOptions in Test ++= Seq("-Yrangepos"),
    libraryDependencies ++= sharedDependencies ++ Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
    )
  ).dependsOn(rhinos).aggregate(rhinos)

lazy val root = Project("qti-converter", file("."))
  .settings(
    scalacOptions in Test ++= Seq("-Yrangepos"),
  libraryDependencies ++= sharedDependencies ++ Seq(
    "commons-io" % "commons-io" % "2.7",
    "org.apache.commons" % "commons-text" % "1.9",
    "com.phloc" % "phloc-css" % "3.8.0",
    "org.mozilla" % "rhino" % "1.7.12",
    "com.typesafe.play" %% "play" % "2.8.2",
    "org.jsoup" % "jsoup" % "1.8.1",
    "com.github.scopt" %% "scopt" % "4.0.0-RC2",
    "com.typesafe.play" %% "play-ahc-ws-standalone" % "2.1.2",
    "joda-time" % "joda-time" % "2.10.6",
   "org.specs2" %% "specs2-core" % "4.10.0" % "it"
),
  parallelExecution in IntegrationTest := false
)
  .configs(IntegrationTest)
  .settings(Defaults.itSettings : _*)
  .settings( fork in IntegrationTest := false )
  .settings(
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoPackage := "qtiConverter"
  )
  .dependsOn(qti, macros).aggregate(qti, macros, rhinos)
