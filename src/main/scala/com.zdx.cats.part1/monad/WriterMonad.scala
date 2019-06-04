package com.zdx.cats.part1.monad

/**
  * Created by zhoudunxiong on 2019/6/4.
  */
object WriterMonad extends App {
  import cats.data.Writer
  import cats.instances.vector._
  import cats.syntax.applicative._
  import cats.syntax.writer._

  Writer(Vector(
    "It is the best time",
    "It is the worse time"
  ), 123)

  type Logged[A] = Writer[Vector[String], A]

  123.pure[Logged]

  Vector("msg1", "msg2", "msg3").tell

  // Writer apply
  val a = Writer(Vector("mesg1", "msg2", "msg3"), 123)

  // writer syntax
  val b = 123.writer(Vector("msg1", "msg2", "msg3"))

  // use value function to extract result
  val result1 = a.value
  // use written function extract log
  val log = b.written

  // use run function extract both log and result

  val (log1, res) = b.run

  //composing and transforming Writers
  val writer1: Writer[Vector[String], Int] = for {
    a <- 1.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 42.writer(Vector("x", "y", "z"))
  } yield {
    a + b
  }

  //writer1.run

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))

  //writer2.run

  //transform both log and result with bimap and mapBoth
  val writer3 = writer1.bimap(
    _.map(_.toUpperCase),
    _ + 1
  )

  //writer3.run

  val writer4 = writer1.mapBoth {(log, res) =>
    (log.map(_.toUpperCase), res + 1)
  }

  //writer4.run


  //clear log with reset and swap log and result with swap

  val writer5 = writer1.reset
  //writer5.run

  val writer6 = writer1.swap
  //writer6.run

  def slow[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial(n : Int): Logged[Int] =  for {
    ans <- slow(if (n == 0) 1.pure[Logged] else factorial(n - 1).map(_ * n))
    _ <- Vector(s"fact $n $ans").tell
  } yield ans


  //factorial(5)

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val Vector((logA, resA), (logB, resB)) = Await.result(Future.sequence(Vector(
    Future(factorial(3).run),
    Future(factorial(3).run)
  )), 5.seconds)

  println()
}
