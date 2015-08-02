/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 3/29/15
 * Time: 9:50 PM
 */

import sbt._

object Dependencies {

  // Libraries
  val scalalTest = "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

  // Projects
  val rootDeps = Seq(scalalTest)
}
