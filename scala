name := "wordle-bot"

version := "0.1.0"

scalaVersion := "2.13.14"

// Main class for running the app 
mainClass in (Compile, run) := Some("Create") // or "Register" depending on what you run

// List of dependencies (equivalent to `build-depends`)
libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client3" %% "core" % "3.9.0",          // HTTP client
  "io.circe" %% "circe-core" % "0.14.6",                         // JSON core
  "io.circe" %% "circe-generic" % "0.14.6",                      // JSON auto-derivation
  "io.circe" %% "circe-parser" % "0.14.6"                        // JSON parsing
)

// Optional: To make sure all modules in src are included
Compile / unmanagedSourceDirectories += baseDirectory.value / "src"
