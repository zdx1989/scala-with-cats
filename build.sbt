organization := "com.zdx.cats"

name := "scala-with-cats"

scalaVersion := "2.12.3"

libraryDependencies +=
  "org.typelevel" %% "cats-core" % "1.0.0"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Ypartial-unification"
)