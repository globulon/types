package com.persgroep.types

import scalaz._
import scala.language.{higherKinds, implicitConversions, postfixOps}
import scalaz.syntax.std.boolean._

trait PropertyName[A]
case class Property[P[_] <: PropertyName[_], O, A](n: P[A], get: O ⇒ A)
//The free applicative describes the props as a continuous description of objects deductible
case class ObjectMeta[P[_] <: PropertyName[_], A](props: Free[({type λ[α] = Property[P, A, α]})#λ, A])

trait Properties {
  implicit final protected def lift[P[_] <: PropertyName[_], O, A](p: Property[P, O, A]): Free[({type λ[α] = Property[P, O, α]})#λ, A] =
    Free.liftF[({type λ[α] = Property[P, O, α]})#λ, A](p)
}

trait Persons { self: Properties ⇒

  final protected def propsToDif(src: Person, tgt: Person) =
    new (({type λ[α] = Property[PersonMeta, Person, α]})#λ ~> ({type λ[α] = State[List[Change], α]})#λ) {
      override def apply[A](fa: Property[PersonMeta, Person, A]): State[List[Change], A] = fa match {
        case Property(PersonName(), get) ⇒ State[List[Change], String] {
          diffs ⇒ (diffs ++ (get(src) != get(tgt)).option(get(tgt)).map(NewName(_)).toList, get(tgt))
        }
        case Property(PersonAge(), get) ⇒ State[List[Change], Int] {
          diffs ⇒ (diffs ++ (get(src) != get(tgt)).option(get(tgt)).map(NewAge(_)).toList, get(tgt))
        }
      }
    }

  final protected def compare: Free[({type λ[α] = Property[PersonMeta, Person, α]})#λ, Unit] = for {
    _ ← Property[PersonMeta, Person, String](PersonName(), _.name)
    _ ← Property[PersonMeta, Person, Int](PersonAge(), _.age)
  } yield ()
}

case class Person(name: String, age: Int)

sealed trait PersonMeta[A] extends PropertyName[A]
case class PersonName() extends PersonMeta[String]
case class PersonAge() extends PersonMeta[Int]

sealed trait Change
case class NewName(v: String) extends Change
case class NewAge(v: Int) extends Change


object Run extends Persons with Properties {
  def main(args: Array[String]): Unit = {
    println(compare.foldMap[({type λ[α] = State[List[Change], α]})#λ](propsToDif(Person("alex", 33), Person("alex acevedo", 33))).run(List.empty)._1)
    println(compare.foldMap[({type λ[α] = State[List[Change], α]})#λ](propsToDif(Person("alex", 33), Person("alex", 35))).run(List.empty)._1)
    println(compare.foldMap[({type λ[α] = State[List[Change], α]})#λ](propsToDif(Person("alex", 33), Person("alex acevedo", 35))).run(List.empty)._1)
  }
}