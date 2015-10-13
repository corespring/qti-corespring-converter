import sbt.Keys._

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

lazy val root = (project in file(".")).settings(
  organization := "org.corespring",
  version := "0.1-SNAPSHOT",
  name := "qti-corespring-converter",
  scalaVersion := "2.11.6",
  libraryDependencies ++= Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  	"org.scalaz" %% "scalaz-core" % "7.0.6",
    "com.typesafe.play" %% "play-json" % "2.3.4",
    "com.typesafe.play" %% "play-ws" % "2.3.4",
    "commons-io" % "commons-io" % "2.4",
    "org.specs2" %% "specs2-core" % "3.6.4" % "test"
  )
)