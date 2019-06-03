package com.zdx.cats.part1.monad



/**
  * Created by zhoudunxiong on 2019/6/2.
  */
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
