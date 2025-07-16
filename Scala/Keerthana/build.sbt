ThisBuild / scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client3" %% "core"           % "3.11.0",
  "com.softwaremill.sttp.client3" %% "circe"          % "3.11.0",
  "io.circe"                       %% "circe-core"     % "0.14.14",
  "io.circe"                       %% "circe-generic"  % "0.14.14",
  "io.circe"                       %% "circe-parser"   % "0.14.14"
)
