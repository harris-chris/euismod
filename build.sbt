import Dependencies._
import scala.sys.process._

ThisBuild / scalaVersion     := "2.13.2"
ThisBuild / version          := "0.1.0"

lazy val SportDate = ProjectRef(
  uri("ssh://git@github.com/chrisharriscjh/sport-date.git#master"), "SportDate"
)

lazy val SportArray = (project in file("."))
  .settings(
    libraryDependencies += scalaTest % Test
  )
  .dependsOn(SportDate)

def removegit = Command.command("removegit"){state =>
  val home = sys.env("HOME")
  val k = ("rm -rf "+ home + "/.sbt/1.0/staging/").!
  state
}

commands ++= Seq(removegit)
