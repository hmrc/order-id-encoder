import sbt.Keys._
import sbt._
import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion
import uk.gov.hmrc.SbtAutoBuildPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning
import uk.gov.hmrc.SbtArtifactory
import uk.gov.hmrc.SbtArtifactory.autoImport.makePublicallyAvailableOnBintray

object HmrcBuild extends Build {

  val appName = "order-id-encoder"

  lazy val microservice = Project(appName, file("."))
    .enablePlugins(Seq(SbtAutoBuildPlugin, SbtGitVersioning, SbtArtifactory) : _*)
    .settings(majorVersion := 1)
    .settings(makePublicallyAvailableOnBintray := true)
    .settings(
      scalaVersion := "2.11.11",
      libraryDependencies ++= AppDependencies(),
      crossScalaVersions := Seq("2.11.11"),
      resolvers := Seq(
        Resolver.bintrayRepo("hmrc", "releases"),
        "typesafe-releases" at "http://repo.typesafe.com/typesafe/releases/"
      )
    )
    .disablePlugins(sbt.plugins.JUnitXmlReportPlugin)
}


private object AppDependencies {

  val compile = Seq(
    "joda-time" % "joda-time" % "2.3",
    "commons-lang" % "commons-lang" % "2.6"
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply() = new TestDependencies {
      override lazy val test = Seq(
        "org.scalatest" %% "scalatest" % "3.0.0" % scope,
        "org.pegdown" % "pegdown" % "1.6.0" % scope,
        "org.scalacheck" %% "scalacheck" % "1.13.4" % scope
      )
    }.test
  }

  def apply() = compile ++ Test()
}