name := "word-snake"

version := "1.0.0"

scalacOptions ++= Seq("-unchecked", "-deprecation")

javacOptions ++= Seq("-Xlint:deprecation")

// seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"
