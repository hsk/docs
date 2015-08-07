libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.2.1"

resolvers += Resolver.sonatypeRepo("snapshots")
scalaVersion := "2.11.5"
libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
  "org.scala-lang.modules" %% "scala-xml" %  "1.0.1",
  "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.1"
)
