package com.zdx.cats.part1.monad_transformers

import com.sun.net.httpserver.Authenticator.Success

/**
  * Created by zhoudunxiong on 2019/6/6.
  */

//
object MonadTransformerTest {

  //composing Monads
  import cats.Monad
  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import cats.instances.option._
  import scala.language.higherKinds

  def compose[M1[_]: Monad] = {
    type Composed[A] = M1[Option[A]]


    new Monad[Composed] {

      override def pure[A](x: A): Composed[A] = x.pure[Option].pure[M1]

      override def flatMap[A, B](fa: Composed[A])(f: (A) => Composed[B]): Composed[B] = ???
//        fa.flatMap {
//          _.fold(Option.empty[B].pure[M1])(f)
//        }

      override def tailRecM[A, B](a: A)(f: (A) => Composed[Either[A, B]]): Composed[B] = ???

    }
  }


  import cats.data.OptionT
  import cats.syntax.applicative._
  import cats.instances.option._
  import cats.instances.list._

  type ListOption[A] = OptionT[List, A]

  val result1: ListOption[Int] = OptionT(List(Option(10)))

  val result2: ListOption[Int] = 32.pure[ListOption]

  val result3 =
    for {
      a <- result1
      b <- result2
    } yield a + b

  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  import cats.instances.either._

  val a = 10.pure[ErrorOrOption]
  val b = 32.pure[ErrorOrOption]
  val c = for {
    x <- a
    y <- b
  } yield x + y

  import cats.data.{OptionT, EitherT}
  import scala.concurrent.Future

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Await
  import scala.concurrent.duration._
  import cats.instances.future._

  val futureEitherOr = for {
    a <- 10.pure[FutureEitherOption]
    b <- 32.pure[FutureEitherOption]
  } yield a + b

  val stack = futureEitherOr.value.value
  val result = Await.result(stack, 3.seconds)

  import cats.data.Writer

  type Logged[A] = Writer[List[String], A]

  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(value) => Writer(List(s"read $str"), Some(value))
      case None => Writer(List(s"failed $str"), None)
    }

  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT
    val result = for {
      x <- OptionT(parseNumber(a))
      y <- OptionT(parseNumber(b))
      z <- OptionT(parseNumber(c))
    } yield x + y + z
    result.value
  }

  //type Response[A] = Future[Either[String, A]]

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(aubot: String): Response[Int] =
    powerLevels.get(aubot) match {
      case Some(value) => EitherT.right[String](Future(value))
      case None => EitherT.left[Int](Future(s"$aubot unreachable"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      l1 <- getPowerLevel(ally1)
      l2 <- getPowerLevel(ally2)
    } yield (l1 + l2) > 15
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    val result = Await.result(canSpecialMove(ally1, ally2).value, 3.seconds)
    result match {
      case Left(str) => str
      case Right(true) => s"$ally1 and $ally2  are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
    }

  }
}

