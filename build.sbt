name := "ATaleOfZIO"

version := "0.1"

scalaVersion := "2.12.11"

mainClass in(Compile, run) := Some( "es.hybride.Main" )

fork in run := true

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-language:higherKinds",
  "-Ypartial-unification")
