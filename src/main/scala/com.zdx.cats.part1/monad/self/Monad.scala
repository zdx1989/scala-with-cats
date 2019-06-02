package com.zdx.cats.part1.monad.self

import scala.language.higherKinds

/**
  * Created by zhoudunxiong on 2019/6/2.
  */
trait Monad[F[_]] {

  def pure[A](value: A): F[A]

  def flatMap[A, B](fa: F[A])(func: A => F[B]): F[B]

  def map[A, B](fa: F[A])(func: A => B): F[B] =
    flatMap(fa) { a => pure(func(a)) }
}
