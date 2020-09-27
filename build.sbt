import Dependencies._
import scala.sys.process._

ThisBuild / scalaVersion     := "2.13.2"
ThisBuild / version          := "0.1.0"

lazy val sportDate = ProjectRef(
  uri("ssh://git@github.com/chrisharriscjh/sport-date.git#master"), "SportDate"
)

lazy val SportArray = (project in file("."))
  .settings(
    libraryDependencies += scalaTest % Test,
    scalacOptions += "-Xlog-implicits",
  )
  .dependsOn(sportDate)

def removegit = Command.command("removegit"){state =>
  val home = sys.env("HOME")
  val k = ("rm -rf "+ home + "/.sbt/1.0/staging/").!
  state
}

libraryDependencies += {
  val version = scalaBinaryVersion.value match {
    case "2.10" => "1.0.3"
    case _ ⇒ "2.2.0"
  }
  "com.lihaoyi" % "ammonite" % version % "test" cross CrossVersion.full
}
libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
)

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue

commands ++= Seq(removegit)
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

// if your project uses multiple Scala versions, use this for cross building
addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full)

// if your project uses both 2.10 and polymorphic lambdas
libraryDependencies ++= (scalaBinaryVersion.value match {
  case "2.10" =>
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full) :: Nil
  case _ =>
    Nil
})
