import sbt._
import sbt.Keys._
import sbt.complete.Parser
import sbt.complete.DefaultParsers._
 
object GitCommandTest extends Build{
 
  lazy val gitCommandParser = {
    (  Space ~> ( token("add") | "commit" | "-v" | "branch" | "push" | "init" | "status" | "tag" | "log" | "checkout" | "rm" | "diff" | "mv" ) ) ~
    ( ( Space ?) ~> ( any *) )
  }
 
  lazy val root = Project(
    "root",
    file("."),
    settings = Defaults.defaultSettings ++ Seq(
      commands ++= Seq(
        Command("git")(_ => gitCommandParser) {case (state, ( cmd , params ) ) =>
          Seq("git",cmd,params.mkString).mkString(" ") ! ;
          state
        }
      )
    )
  )
 
}
