import sbt._
import Keys._
//val extracted: Extracted = Project.extract(state)
//import extracted._

object BuildSettings extends Build {
  lazy val simplehdl = Project("hdl", file("simple-hdl/src"))
  lazy val fsm = Project("fsm", file("src/fsm")).dependsOn(simplehdl)
}
