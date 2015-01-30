name := "scala-state-training"

scalaVersion := "2.11.1"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-optimise",
  "-explaintypes",
  "-Xcheckinit",
  "-Xfatal-warnings",
  "-Xlint",
  "-Xverify",
  "-Yclosure-elim",
  "-Ydead-code",
  "-Yinline",
  "-Ywarn-all"
)

triggeredMessage := (_ => Watched.clearScreen)

libraryDependencies +=
  "org.scalaz" %% "scalaz-core" % "7.0.0"