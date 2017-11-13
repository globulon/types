package com.persgroep.types

import scala.language.{higherKinds, implicitConversions, postfixOps}
import scalaz.syntax.std.boolean._
import scalaz.{Free, _}

trait PropertyName[A]
//The free applicative describes the props as a continuous description of objects deductible
case class Property[P[_] <: PropertyName[_], O, A](n: P[A], get: O ⇒ A)
case class ObjectMeta[P[_] <: PropertyName[_], A](props: Free[({type λ[α] = Property[P, A, α]})#λ, A])

trait Properties {
  implicit final protected def lift[P[_] <: PropertyName[_], O, A](p: Property[P, O, A]): Free[({type λ[α] = Property[P, O, α]})#λ, A] =
    Free.liftF[({type λ[α] = Property[P, O, α]})#λ, A](p)
}

trait Persons { self: Properties ⇒
  protected type PersonProp[A] = Property[PersonMeta, Person, A]
  protected type MakeChange[A] = A ⇒ Change[A]
  protected type Comparison[A] = Property[PersonMeta, Person, A]
  protected type StoreChange[A] = State[List[Change[_]], A]

  protected def personName: Comparison[String] = Property[PersonMeta, Person, String](PersonName(), _.name)
  protected def personAge: Comparison[Int]  = Property[PersonMeta, Person, Int](PersonAge(), _.age)


  final protected def propToChange = new (PersonMeta ~> MakeChange) {
    override def apply[A](fa: PersonMeta[A]) = fa match {
      case PersonName() ⇒ NewName(_)
      case PersonAge()  ⇒ NewAge(_)
    }
  }

  final protected def propsToChange(src: Person, tgt: Person) =
    new (PersonProp ~> StoreChange) {
      override def apply[A](fa: PersonProp[A]): StoreChange[A] = fa match {
        case Property(name, get) ⇒ State[List[Change[_]], A] {
          diffs ⇒ (diffs ++ (get(src) != get(tgt)).option(get(tgt)).map(propToChange(name)).toList, get(tgt))
        }
      }
    }

  final protected def run(comparison: Free[({type λ[α] = Property[PersonMeta, Person, α]})#λ, Unit])(src: Person, tgt: Person) =
    comparison.foldMap[({type λ[α] = State[List[Change[_]], α]})#λ](propsToChange(src, tgt)).run(List.empty)._1

}

case class Person(name: String, age: Int)

sealed trait PersonMeta[A] extends PropertyName[A]
case class PersonName() extends PersonMeta[String]
case class PersonAge() extends PersonMeta[Int]

sealed trait Change[A]
case class NewName(v: String) extends Change[String]
case class NewAge(v: Int) extends Change[Int]

object Run extends Persons with Properties {
  private def comparison = for {
    _ ← personName
    _ ← personAge
  } yield ()


  def main(args: Array[String]): Unit = {
    println(run(comparison)(Person("alex", 33), Person("alex acevedo", 33)))
    println(run(comparison)(Person("alex", 33), Person("alex", 35)))
    println(run(comparison)(Person("alex", 33), Person("alex acevedo", 35)))
  }
}