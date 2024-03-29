package com.zdx.cats.part2.dataValidation

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated._
import cats.syntax.validated._
import cats.syntax.semigroup._
import cats.syntax.apply._

/**
  * Created by zhoudunxiong on 2019/6/15.
  */
sealed trait Predicate[E, A] {

  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Or(this, that)

  def apply(value: A)(implicit se: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(func) => func(value)
      case And(left, right) =>
        (left(value), right(value)).mapN((_, _) => value)
      case Or(left, right) =>
        left(value) match {
          case Valid(a) => a.valid[E]
          case Invalid(e1) =>
            right(value) match {
              case Valid(a) => a.valid[E]
              case Invalid (e2) => (e1 |+| e2).invalid[A]
            }
        }
    }

}


object Predicate {

  final case class And[E, A](left: Predicate[E, A],
                             right: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A],
                            right: Predicate[E, A]) extends Predicate[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
    Pure(a => if(fn(a)) a.valid else err.invalid)


  val a = Pure[List[String], Int]{ v =>
    if (v > 2) v.valid[List[String]]
    else List("Muste be > 2").invalid[Int]
  }

  val b = Pure[List[String], Int] { v =>
    if (v < -2) v.valid[List[String]]
    else List("Muste be < -2").invalid[Int]
  }

  val check: Predicate[List[String], Int] = a and b
}
