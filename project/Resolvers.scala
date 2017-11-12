import sbt._

import scala.language.{higherKinds, implicitConversions, postfixOps}

trait Resolvers {
  final protected def repos = List(
    "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
    "sonatype-oss-public" at "https://oss.sonatype.org/content/groups/public/", Resolver.bintrayRepo("atrepo", "atrepo"),
    "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/")
}