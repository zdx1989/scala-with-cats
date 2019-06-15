package com.zdx.cats.part2.mapReduce


/**
  * Created by zhoudunxiong on 2019/6/15.
  */
object FoldMap {

  // implementing foldMap
  import cats.Monoid
  import cats.syntax.semigroup._

  def foldMap[A, B: Monoid](va: Vector[A])(func: A => B): B = {
    va.map(func).foldLeft(Monoid[B].empty)(_ |+| _)
  }

  def foldMap1[A, B: Monoid](va: Vector[A])(func: A => B): B =
    va.foldLeft(Monoid[B].empty)(_ |+| func(_))

  // Parallelising foldMap
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  val future1 = Future(
    (1 to 100).toList.foldLeft(0)(_ + _)
  )

  val future2 = Future(
    (100 to 200).toList.foldLeft(0)(_ + _)
  )

  val future3 = future1.map(_.toString)

  val future4 = for {
    a <- future1
    b <- future2
  } yield a + b

  //convert List[Future[A]] to Future[List[A]] using Future.sequence or instance of Traverse
  Future.sequence(List(future1, future2, future4))

  import cats.syntax.traverse._
  import cats.instances.future._
  import cats.instances.list._

  List(future1, future2, future4).sequence

  //block the future until the result is available
  import scala.concurrent.Await
  import scala.concurrent.duration._
  val result1 = Await.result(future1, 1.seconds)

  //Monad and Monoid implementations for Future
  import cats.{Monoid, Monad}
  import cats.instances.future._
  import cats.instances.int._


  Monad[Future].pure(42)

  Monoid[Future[Int]].combine(future1, future2)


  //dividing work
  val codes = Runtime.getRuntime.availableProcessors()

  //partition sequence using grouped
  val groupList = (1 to 10).toList.grouped(4).toList

}
