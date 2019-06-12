package com.zdx.cats.part1.flodable_traverses

/**
  * Created by zhoudunxiong on 2019/6/10.
  */
object TraversesTest extends App {

  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val hostNames = List(
    "www.baidu.com",
    "www.hupu.com",
    "www.zhihu.com"
  )

  def getTime(hostname: String): Future[Int] =
    Future(hostname.length * 60)

  val allTimes: Future[List[Int]] =
    hostNames.foldLeft(Future(List.empty[Int])) { (accum, item) =>
      for {
        times <- accum
        t <- getTime(item)
      } yield times :+ t
    }

  val timeRes = Await.result(allTimes, 2.seconds)
  println(timeRes)

  val allTimes1: Future[List[Int]] =
    Future.traverse(hostNames)(getTime)

  val timeRes1 = Await.result(allTimes1, 2.seconds)
  println(timeRes1)


  def travers[A, B](list: List[A])(func: A => Future[B]): Future[List[B]] =
    list.foldLeft(Future(List.empty[B])) { (accum, item) =>
      for {
        lb <- accum
        a <- func(item)
      } yield lb :+ a
    }

  def sequence[A](list: List[Future[A]]): Future[List[A]] =
    travers(list)(identity)

  //Traversing in applicative
  import cats.Applicative
  import cats.instances.future._
  import cats.syntax.applicative._
  import cats.syntax.apply._

  def oldCombine(accum: Future[List[Int]],
                 host: String): Future[List[Int]] = {
    for {
      accum <- accum
      host <- getTime(host)
    } yield accum :+ host
  }

  def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
    (accum, getTime(host)).mapN(_ :+ _)

  import scala.language.higherKinds

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity)


}
