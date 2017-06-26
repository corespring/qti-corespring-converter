import sbt._
import sbt.Keys._
import org.corespring.sbt.repo.RepoAuthPlugin.Keys._

resolvers in ThisBuild ++= Seq(
  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
  "Corespring releases" at "http://repository.corespring.org/artifactory/ivy-releases/"
)

fork in Test := true

organization in ThisBuild := "org.corespring"
scalaVersion in ThisBuild := "2.10.5"

val sharedDependencies = Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.6",
  "com.typesafe.play" %% "play-json" % "2.2.1",
  "org.specs2" %% "specs2" % "2.1.1" % "test"
)

val rhinoJs = "org.mozilla" % "rhino" % "1.7.6"


lazy val qti = Project("corespring-qti", file("lib/qti"))
  .settings(
    libraryDependencies ++= sharedDependencies ++ Seq(
      "org.corespring.forks.scalapeno" %% "rhinos" % "0.6.1"
    ),
    publishTo := authPublishTo.value
  )

lazy val root = Project("qti-corespring-converter", file("."))
  .settings(
  libraryDependencies ++= sharedDependencies ++ Seq(
    "commons-io" % "commons-io" % "2.4",
    "com.phloc" % "phloc-css" % "3.7.6",
    "org.mozilla" % "rhino" % "1.7R4",
    "org.specs2" %% "specs2" % "2.1.1" % "it,test",
    "com.typesafe.play" %% "play" % "2.2.1",
    "org.jsoup" % "jsoup" % "1.8.1",
    "com.github.scopt" %% "scopt" % "3.6.0"
  ),
  publishTo := authPublishTo.value,
  parallelExecution in IntegrationTest := false
)
  .configs(IntegrationTest)
  .settings(Defaults.itSettings : _*)
  .settings(
    fork in IntegrationTest := false
    )
  .dependsOn(qti).aggregate(qti)
