resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

scalaVersion := "2.11.7"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.4"
libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"