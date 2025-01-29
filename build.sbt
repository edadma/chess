ThisBuild / licenses += "ISC"  -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme      := Some("semver-spec")
ThisBuild / evictionErrorLevel := Level.Warn

publish / skip := true

lazy val chess = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name         := "chess",
    version      := "0.0.1",
    scalaVersion := "3.6.2",
    organization := "io.github.edadma",
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
      "io.github.edadma"  %%% "logger"          % "0.0.6",
      "org.scalatest"     %%% "scalatest"       % "3.2.19" % "test",
      "com.lihaoyi"       %%% "pprint"          % "0.9.0"  % "test",
    ),
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
//    Test / scalaJSUseMainModuleInitializer := true,
//    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer        := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    publishMavenStyle      := true,
    Test / publishArtifact := false,
    licenses += "ISC"      -> url("https://opensource.org/licenses/ISC"),
  )
