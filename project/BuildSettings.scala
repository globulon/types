import bintray.BintrayPlugin.autoImport._
import sbt.Keys._
import sbt._

import scala.language.{higherKinds, implicitConversions, postfixOps}

trait BuildSettings { self: Versions ⇒
  final protected def buildSettings: Seq[Setting[_]] = Defaults.coreDefaultSettings ++ Seq(
    organization  := "com.autotrack",
    version       := appVer,
    scalaVersion  := scalaVer,
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-target:jvm-1.8"),
    ivyValidate   := false,
    fork in Test  := true,
    credentials += Credentials(Path.userHome / ".ivy2" / ".atrepo"),
    bintrayOrganization := Some("atrepo"),
    bintrayRepository := "atrepo",
    shellPrompt   := {  Project.extract(_).currentRef.project + "> " }
  )

  final protected def allJavaOptions: Seq[sbt.Def.SettingsDefinition] = Seq(
    javaOptions in Test += "-Dconfig.file=src/test/resources/test.conf"
  )
}
