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
  "com.typesafe.play" %% "play-json" % "2.9.0"
)

//def buildSettings = Defaults.defaultSettings ++ releaseSettings ++ Seq(
//  organization := "org.corespring.forks.scalapeno",
//  scalaVersion := "2.10.0",
//  resolvers ++= Seq(
//    "typesafe releases" at "http://repo.typesafe.com/typesafe/releases/",
//    "typesafe snapshots" at "http://repo.typesafe.com/typesafe/snapshots/"
//  ),
//  libraryDependencies ++= Seq(
//    "org.mozilla" % "rhino" % "1.7R4",
//    "com.typesafe.play" %% "play-json" % "2.2.0" % "provided",
//    "org.slf4j" %  "slf4j-api" % "1.6.4",
//    "ch.qos.logback" % "logback-classic" % "1.0.0" % "provided",
//    "org.specs2" %% "specs2" % "1.12.3" % "test",
//    "junit" % "junit" % "4.8.2" % "test",
//    "org.mockito" % "mockito-all" % "1.9.0" % "test"
//  ),
//  credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
//  publishTo <<= version {
//    (v: String) =>
//      def isSnapshot = v.trim.contains("-")
//      val base = "http://repository.corespring.org/artifactory"
//      val repoType = if (isSnapshot) "snapshot" else "release"
//      val finalPath = base + "/ivy-" + repoType + "s"
//      Some( "Artifactory Realm" at finalPath )
//  },
//  unmanagedBase <<= baseDirectory { base => base / "lib" },
//  scalacOptions := Seq("-deprecation", "-encoding", "utf8")
//)

lazy val rhinos = Project("rhinos", base = file("lib/rhinos"))
  .settings(
    libraryDependencies ++= Seq(
      "org.mozilla" % "rhino" % "1.7.12",
      "com.typesafe.play" %% "play-json" % "2.9.0" % "provided",
      "org.slf4j" %  "slf4j-api" % "1.6.4",
      "ch.qos.logback" % "logback-classic" % "1.0.0" % "provided",
    )
  )
  //, settings = buildSettings )
lazy val macros = Project("macros", file("lib/macros"))
  .settings(
    libraryDependencies ++= Seq( "org.scala-lang" % "scala-reflect" % "2.13.3")
  )

lazy val qti = Project("corespring-qti", file("lib/qti"))
  .settings(
    libraryDependencies ++= sharedDependencies ++ Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
    )
  ).dependsOn(rhinos).aggregate(rhinos)

lazy val root = Project("qti-corespring-converter", file("."))
  .settings(
  libraryDependencies ++= sharedDependencies ++ Seq(
    "commons-io" % "commons-io" % "2.7",
    "com.phloc" % "phloc-css" % "3.8.0",
    "org.mozilla" % "rhino" % "1.7.12",
    "com.typesafe.play" %% "play" % "2.8.2",
    "org.jsoup" % "jsoup" % "1.8.1",
    "com.github.scopt" %% "scopt" % "4.0.0-RC2",
    "com.typesafe.play" %% "play-ahc-ws-standalone" % "2.1.2",
    "joda-time" % "joda-time" % "2.10.6"
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
  .dependsOn(qti, macros).aggregate(qti, macros)
