name := "chess"

version := "0.1"

scalaVersion := "2.12.3"

crossScalaVersions := Seq( "2.11.11" )

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "xyz.hyperreal"

//resolvers += Resolver.sonatypeRepo( "snapshots" )

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.0" % "test",
	"org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

libraryDependencies ++= Seq(
	"org.scala-lang.modules" %% "scala-swing" % "2.0.0"
)

libraryDependencies ++= Seq(
	"xyz.hyperreal" %% "json" % "0.7",
	"xyz.hyperreal" %% "table" % "0.10",
	"xyz.hyperreal" %% "options" % "0.2"
)

mainClass in (Compile, run) := Some( "xyz.hyperreal." + name.value + ".Main" )

mainClass in assembly := Some( "xyz.hyperreal." + name.value + ".Main" )

//Revolver.settings
//
//mainClass in Revolver.reStart := Some("xyz.hyperreal." + name.value + ".ServerMain" )

assemblyJarName in assembly := name.value + "-" + version.value + ".jar"

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/edadma/" + name.value))

pomExtra :=
	<scm>
		<url>git@github.com:edadma/{name.value}.git</url>
		<connection>scm:git:git@github.com:edadma/{name.value}.git</connection>
	</scm>
		<developers>
			<developer>
				<id>edadma</id>
				<name>Edward A. Maxedon, Sr.</name>
				<url>https://github.com/edadma</url>
				<id>emaxedon</id>
				<name>Edward W. Maxedon, Jr.</name>
				<url>https://github.com/emaxedon</url>
			</developer>
		</developers>
