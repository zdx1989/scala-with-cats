package com.zdx.cats.part1.monad



/**
  * Created by zhoudunxiong on 2019/6/2.
  */

// Monad --> def flatMap[F[_], A, B](fa: F[A])(func: A => F[B]): F[B]
// Monad extend applicative --> def pure[F[_], A](a: A): F[A]
// val -- Now --> eager, memorized
// lazy val -- Later --> lazy, memorized
// def -- Always --> lazy, not memorized
// MonadError
// Eval Monad
// Writer Monad logging; type Writer[W, A] = WriterT[Id, W, A]
// Reader Monad dependence injection;
// State Monad: transform input state to out state and compute a result State => (State, Result)

object MonadTest {

  import scala.language.higherKinds
  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.syntax.either._


  def sumSquare[F[_] : Monad](fa: F[Int], fb: F[Int]): F[Int] =
    for {
      a <- fa
      b <- fb
    } yield a * a + b * b


  def countPositive(list: List[Int]): Either[String, Int] =
    list.foldLeft(0.asRight[String]) { (acc, num) =>
      if (num > 0)
        acc.map(_ + 1)
      else
        "negative stoppping".asLeft[Int]
    }

}
