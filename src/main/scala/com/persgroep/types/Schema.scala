package com.persgroep.types

import scalaz._
import scala.language.{higherKinds, implicitConversions, postfixOps}
import scalaz.syntax.std.boolean._

sealed trait Schema[A]

trait PropertyName[A]

case class StringSchema() extends Schema[String]

case class IntSchema() extends Schema[Int]

case class Property[P[_] <: PropertyName[_], O, A](schema: Schema[A], n: P[A], get: O ⇒ A)

//The free applicative describes the props as a continuous description of objects deductible
case class ObjectMeta[P[_] <: PropertyName[_], A](props: Free[({type λ[α] = Property[P, A, α]})#λ, A]) extends Schema[A]

trait Schemas {
  implicit final protected def lift[P[_] <: PropertyName[_], O, A](p: Property[P, O, A]): Free[({type λ[α] = Property[P, O, α]})#λ, A] =
    Free.liftF[({type λ[α] = Property[P, O, α]})#λ, A](p)
}

trait Persons { self: Schemas ⇒
  sealed trait Diff
  case class NameChange(v: String) extends Diff
  case class AgeChange(v: Int) extends Diff


  final protected def propsToDif(src: Person, tgt: Person) =
    new (({type λ[α] = Property[PersonProp, Person, α]})#λ ~> ({type λ[α] = State[List[Diff], α]})#λ) {
      override def apply[A](fa: Property[PersonProp, Person, A]): State[List[Diff], A] = fa match {
        case Property(StringSchema(), PersonName(), get) ⇒ State[List[Diff], String] {
          diffs ⇒ (diffs ++ (get(src) != get(tgt)).option(get(tgt)).map(NameChange(_)).toList, get(tgt))
        }
        case Property(IntSchema(), PersonAge(), get) ⇒ State[List[Diff], Int] {
          diffs ⇒ (diffs ++ (get(src) != get(tgt)).option(get(tgt)).map(AgeChange(_)).toList, get(tgt))
        }
      }
    }

  final protected def compare: Free[({type λ[α] = Property[PersonProp, Person, α]})#λ, Unit] = for {
    _ ← Property[PersonProp, Person, String](StringSchema(), PersonName(), _.name)
    _ ← Property[PersonProp, Person, Int](IntSchema(), PersonAge(), _.age)
  } yield ()
}

case class Person(name: String, age: Int)

sealed trait PersonProp[A] extends PropertyName[A]
case class PersonName() extends PersonProp[String]
case class PersonAge() extends PersonProp[Int]

object Run extends Persons with Schemas {
  def main(args: Array[String]): Unit = {
    println(compare.foldMap[({type λ[α] = State[List[Diff], α]})#λ](propsToDif(Person("alex", 33), Person("alex acevedo", 33))).run(List.empty)._1)
    println(compare.foldMap[({type λ[α] = State[List[Diff], α]})#λ](propsToDif(Person("alex", 33), Person("alex", 35))).run(List.empty)._1)
    println(compare.foldMap[({type λ[α] = State[List[Diff], α]})#λ](propsToDif(Person("alex", 33), Person("alex acevedo", 35))).run(List.empty)._1)

  }
}