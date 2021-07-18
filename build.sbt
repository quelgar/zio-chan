
name := "zio-chan"

scalaVersion := "3.0.0"

scalacOptions := List(
    "-Yexplicit-nulls",
    "-language:strictEquality",
    "-source", "future"
)

libraryDependencies += "dev.zio" %% "zio-streams" % "1.0.9"
