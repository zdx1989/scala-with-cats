package com.zdx.cats.part1.monad



/**
  * Created by zhoudunxiong on 2019/6/2.
  */
object MonadTest {

  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import scala.language.higherKinds
  import cats.Id

  def sumSquare[F[_] : Monad](fa: F[Int], fb: F[Int]): F[Int] =
    for {
      a <- fa
      b <- fb
    } yield a * a + b * b

}
