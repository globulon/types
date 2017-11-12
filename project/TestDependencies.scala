import sbt._

import scala.language.{higherKinds, implicitConversions, postfixOps}

trait TestDependencies { self: Versions =>
  final protected def scalaTest: ModuleID = "org.scalatest" %% "scalatest" % scalaTestVer % Test withSources()
  final protected def atFpTesting: ModuleID = "com.persgroep" %% "at-fp-testing" % atVersion % Test withSources()
}