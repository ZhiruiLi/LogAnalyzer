import java.io.File

name := "LogAnalyzer"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.92-R10"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
libraryDependencies += "com.typesafe.play" % "play-json_2.11" % "2.4.0-M2"

val sep = File.separator

unmanagedJars in Compile += {
  val ps = new sys.SystemProperties
  val jh = ps("java.home")
  Attributed.blank(file(jh) / s"lib${sep}ext${sep}jfxrt.jar")
}

assemblyOutputPath in assembly := new File(s".${sep}run.jar")
