package com.zdx.cats.part2.dataValidation

import cats.data.Validated
import cats.Semigroup
/**
  * Created by zhoudunxiong on 2019/6/15.
  */
sealed trait Check[E, A, B] {

  def apply(value: A)(implicit sa: Semigroup[E]): Validated[E, B]

  def map[C](func: B => C): Check[E, A, C] =
    Map[E, A, B, C](this, func)

  def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] =
    FlatMap[E, A, B, C](this, func)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
    AndThen[E, A, B, C](this, that)
}

final case class Pure[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {

  def apply(in: A)(implicit sa: Semigroup[E]): Validated[E, A] =
    pred(in)
}

final case class Map[E, A, B, C](check: Check[E, A, B],
                                 func: B => C) extends Check[E, A, C] {

  def apply(in : A)(implicit sa: Semigroup[E]): Validated[E, C] =
    check.apply(in).map(func)
}

final case class FlatMap[E, A, B, C](check: Check[E, A, B],
                                     func: B => Check[E, A, C]) extends Check[E, A, C] {

  def apply(in: A)(implicit sa: Semigroup[E]): Validated[E, C] = {
    check(in).withEither(_.flatMap(b => func(b)(in).toEither))
  }
}

final case class AndThen[E, A, B, C](
                                      check1: Check[E, A, B],
                                      check2: Check[E, B, C]) extends Check[E, A, C] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
    check1(a).withEither(_.flatMap(b => check2(b).toEither))
}