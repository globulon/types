import sbt._

import scala.language.{higherKinds, implicitConversions, postfixOps}

trait Dependencies { self: Versions =>
  final protected def atFp: ModuleID = "com.persgroep" %% "at-fp" % atVersion withSources()
}