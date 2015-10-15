import sbt.Keys._

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

lazy val qti = (project in file("lib/qti")).settings(
  organization := "org.corespring",
  version := "0.1-SNAPSHOT",
  name := "corespring-qti",
  scalaVersion := "2.10.5",
  libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.0.6",
    "com.typesafe.play" %% "play-json" % "2.3.4",
    "org.mozilla" % "rhino" % "1.7R4",
    "org.corespring.forks.scalapeno" %% "rhinos" % "0.6.1"
  )
)

lazy val root = (project in file(".")).settings(
  organization := "org.corespring",
  version := "0.1-SNAPSHOT",
  name := "qti-corespring-converter",
  scalaVersion := "2.10.5",
  libraryDependencies ++= Seq(
  	"org.scalaz" %% "scalaz-core" % "7.0.6",
    "com.typesafe.play" %% "play-json" % "2.3.4",
    "com.typesafe.play" %% "play-ws" % "2.3.4",
    "commons-io" % "commons-io" % "2.4",
    "com.phloc" % "phloc-css" % "3.7.6",
    "org.mozilla" % "rhino" % "1.7R4",
    "org.specs2" %% "specs2" % "2.1.1" % "test"
  )
).dependsOn(qti).aggregate(qti)
