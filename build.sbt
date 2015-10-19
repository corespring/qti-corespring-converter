import sbt.Keys._

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

credentials += {
  val envCredentialsPath = System.getenv("CREDENTIALS_PATH")
  val path = if (envCredentialsPath != null) envCredentialsPath else Seq(Path.userHome / ".ivy2" / ".credentials").mkString
  val f: File = file(path)
  if (f.exists()) {
    println("[credentials] using credentials file")
    Credentials(f)
  } else {
    def repoVar(s: String) = System.getenv("ARTIFACTORY_" + s)
    val args = Seq("REALM", "HOST", "USER", "PASS").map(repoVar)
    println("[credentials] args: " + args)
    Credentials(args(0), args(1), args(2), args(3))
  }
}

fork in Test := true

lazy val qti = (project in file("lib/qti")).settings(
  organization := "org.corespring",
  version := "0.5-SNAPSHOT",
  name := "corespring-qti",
  scalaVersion := "2.10.5",
  libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.0.6",
    "com.typesafe.play" %% "play-json" % "2.2.1",
    "org.mozilla" % "rhino" % "1.7R4",
    "org.corespring.forks.scalapeno" %% "rhinos" % "0.6.1"
  ),
  publishTo <<= version {
    (v: String) =>
      def isSnapshot = v.trim.contains("-")
      val base = "http://repository.corespring.org/artifactory"
      val repoType = if (isSnapshot) "snapshot" else "release"
      val finalPath = base + "/ivy-" + repoType + "s"
      Some("Artifactory Realm" at finalPath)
  }
)

lazy val root = (project in file(".")).settings(
  organization := "org.corespring",
  version := "0.5-SNAPSHOT",
  name := "qti-corespring-converter",
  scalaVersion := "2.10.5",
  libraryDependencies ++= Seq(
  	"org.scalaz" %% "scalaz-core" % "7.0.6",
    "com.typesafe.play" %% "play" % "2.2.1",
    "commons-io" % "commons-io" % "2.4",
    "com.phloc" % "phloc-css" % "3.7.6",
    "org.mozilla" % "rhino" % "1.7R4",
    "org.specs2" %% "specs2" % "2.1.1" % "test"
  ),
  publishTo <<= version {
    (v: String) =>
      def isSnapshot = v.trim.contains("-")
      val base = "http://repository.corespring.org/artifactory"
      val repoType = if (isSnapshot) "snapshot" else "release"
      val finalPath = base + "/ivy-" + repoType + "s"
      Some("Artifactory Realm" at finalPath)
  }
).dependsOn(qti).aggregate(qti)
