name := "ATaleOfZIO"

version := "0.1"

scalaVersion := "2.12.11"

mainClass in(Compile, run) := Some( "es.hybride.Main" )

fork in run := true


val zioVersion        = "1.0.0-RC18-2"
libraryDependencies ++= Seq(
  "dev.zio"       %% "zio"                    % zioVersion
)

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-language:higherKinds",
  "-Ypartial-unification")
