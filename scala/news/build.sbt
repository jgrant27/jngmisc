scalaVersion := "2.13.3"

libraryDependencies += "com.softwaremill.sttp.client" %% "core"  % "2.2.6"
libraryDependencies += "com.softwaremill.sttp.client" %% "circe" % "2.2.6"

val circeVersion = "0.13.0"
libraryDependencies ++= Seq(
  "io.circe"  %% "circe-core"     % circeVersion,
  "io.circe"  %% "circe-generic"  % circeVersion,
  "io.circe"  %% "circe-parser"   % circeVersion
)
