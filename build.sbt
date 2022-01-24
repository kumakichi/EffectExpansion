ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "EffectExpansion",
    idePackagePrefix := Some("side.effect.free")
  )

libraryDependencies += "org.soot-oss"     % "soot"          % "4.2.1"
libraryDependencies += "org.slf4j"        % "slf4j-simple"  % "1.7.32"
libraryDependencies += "org.scala-graph" %% "graph-core"    % "1.13.3"
libraryDependencies += "org.scala-graph" %% "graph-dot"     % "1.13.3"
libraryDependencies += "org.scalatest"   %% "scalatest"     % "3.2.9"
libraryDependencies += "org.json4s"      %% "json4s-native" % "4.0.3"

compileOrder := CompileOrder.JavaThenScala

javacOptions ++= Seq(
  "-source",
  "1.8",
  "-target",
  "1.8"
)
