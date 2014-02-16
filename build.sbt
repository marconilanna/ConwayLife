name := "ConwayLife"

version := "1.0"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.0" % "test"
)

scalacOptions ++= Seq(
    "-deprecation"
  , "-feature"
  , "-unchecked"
  , "-Xfatal-warnings"
  , "-Xlint"
  , "-Yno-adapted-args"
  , "-Ywarn-all"
  , "-Ywarn-dead-code"
)

scalaSource in Compile <<= baseDirectory(_ / "src")

javaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test")

javaSource in Test <<= baseDirectory(_ / "test")

showSuccess := true

showTiming := true
