import SonatypeKeys._

sonatypeSettings

name := "geow"

organization := "io.plasmap"

version := "0.3.2-SNAPSHOT"

scalaVersion := "2.11.6"

homepage := Some(url("http://www.plasmap.io"))

licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

scmInfo := Some(ScmInfo(
    url("https://github.com/plasmap/geow"),
    "scm:git:git@github.com/plasmap/geow.git",
    Some("scm:git:git@github.com/plasmap/geow.git")))

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

pomExtra := (
  <developers>
    <developer>
      <id>janschultecom</id>
      <name>Jan Schulte</name>
      <url>www.plasmap.io</url>
    </developer>
    <developer>
      <id>i-am-the-slime</id>
      <name>Mark Eibes</name>
      <url>www.plasmap.io</url>
    </developer>
  </developers>
)

pomIncludeRepository := { _ => false }

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.2",
  "org.scalaz" %% "scalaz-concurrent" % "7.1.2",
  "com.github.alexarchambault" %% "argonaut-shapeless_6.1" % "0.1.1",
  "org.json4s" %% "json4s-native" % "3.2.11",
  "org.scala-lang" %% "scala-pickling" % "0.9.1",
  "org.apache.commons" % "commons-compress" % "1.8.1",
  "org.scala-lang" % "scala-xml" % "2.11.0-M4",
  "joda-time" % "joda-time" % "2.3",
  "org.hamcrest" % "hamcrest-all" % "1.3" % "test",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test",
  "org.specs2" %% "specs2" % "2.4.11" % "test",
  "org.mockito" % "mockito-core" % "1.9.5" % "test",
  "org.mockito" % "mockito-all" % "1.9.5" % "test",
  "junit" % "junit" % "4.11" % "test"
)

