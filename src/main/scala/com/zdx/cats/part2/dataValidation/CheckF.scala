package com.zdx.cats.part2.dataValidation

import cats.Semigroup
import cats.syntax.semigroup._
import cats.syntax.either._

/**
  * Created by zhoudunxiong on 2019/6/15.
  */
case class CheckF[E, A](func: A => Either[E, A]) {

  def apply(value: A): Either[E, A] = func(value)

  def and(that: CheckF[E, A])(implicit se: Semigroup[E]): CheckF[E, A] = CheckF[E, A] { a =>
    (this(a), that(a)) match {
      case (Right(a1), Right(a2)) => a.asRight
      case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
      case (Left(e), Right(_)) => e.asLeft
      case (Right(_), Left(e)) => e.asLeft
    }
  }

}

object CheckF {

  import cats.instances.list._

  val a = CheckF[List[String], Int] { v =>
    if (v > 2) v.asRight
    else List("Muste be > 2").asLeft
  }

  val b = CheckF[List[String], Int] { v =>
    if (v < -2) v.asRight
    else List("Muste be < -2").asLeft
  }

  val check: CheckF[List[String], Int] = a and b


}
