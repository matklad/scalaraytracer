name := "scalaraytracer"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)

scalacOptions ++= Seq("-feature", "-deprecation")

fullRunTask(TaskKey[Unit]("bench"), Test, "Bench")

mainClass in (Compile, run) := Some("Main")

fork in run := true