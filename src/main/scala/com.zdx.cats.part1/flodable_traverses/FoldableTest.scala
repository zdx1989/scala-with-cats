package com.zdx.cats.part1.flodable_traverses

import cats.Foldable

/**
  * Created by zhoudunxiong on 2019/6/9.
  */

// Foldable --> foldleft, foldright, combineAll, foldMap
object FoldableTest extends App{

  def show[A](list: List[A]): String =
    list.foldLeft("nil") { (accum, item) =>
      s"$item and then $accum"
    }

  def concatLeft[A](list: List[A]): List[A] =
    list.foldLeft(List.empty[A]) { (accum, item) =>
      item :: accum
    }

  def concatRight[A](list: List[A]): List[A] =
    list.foldRight(List.empty[A]) { (item , accum) =>
      item :: accum
    }

  def map[A, B](list: List[A])(func: A => B): List[B] =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) :: accum
    }

  def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) ::: accum
    }

  def filter[A](list: List[A])(func: A => Boolean): List[A] =
    list.foldRight(List.empty[A]) { (item, accum) =>
      if (func(item)) item :: accum
      else accum
    }

  import cats.Monoid
  def sum[A](list: List[A])(implicit ma: Monoid[A]): A =
    list.foldRight(ma.empty)(ma.combine)

  def sumWithNumeric[A](list: List[A])(implicit na: Numeric[A]): A =
    list.foldRight(na.zero)(na.plus)

  //traversing with Vectors

  def listSequence[A](list: List[Vector[A]]): Vector[List[A]] = {

    def loop(index: Int, res: Vector[List[A]]): Vector[List[A]] = index match {
      case m if m == list.length => res
      case n =>
        if (list(n).isEmpty) loop(n + 1, res)
        else {
          loop(n + 1, res :+ list(n).toList)
        }
    }

    loop(0, Vector())
  }

  def listSequence1[A](list: List[Vector[A]]): Vector[List[A]] = {
    list.foldLeft(Vector.empty[List[A]]) { (accum, item) =>
      accum :+ item.toList
    }
  }

  val res = listSequence1(List(Vector(1, 2), Vector(3, 4)))

  println(res)

  //Foldable in cats
  import cats.Foldable
  import cats.instances.list._

  val list = List(1, 2, 3)

  val foldRes = Foldable[List].foldLeft(list, 0)(_ + _)

  import cats.instances.option._

  val maybe = Option(1)

  val foldOption = Foldable[Option].foldLeft(maybe, 10)(_ * _)

  //the implementation of foldRight for Stream is not stack safe
  import cats.Eval
  import cats.instances.stream._

  val stream = (1 to 100000).toStream
  val foldStream = stream.foldRight(0L)(_ + _)

  val eval: Eval[Long] = Foldable[Stream].foldRight(stream, Eval.now(0L)) { (item, accmu) =>
    accmu.map(_ + item)
  }

  val foldEval = eval.value


  import cats.instances.list._
  Foldable[Option].nonEmpty(Some(1))
  Foldable[List].find(List(1, 2, 3))(_ > 2)

  import cats.instances.int._
  Foldable[List].combineAll(List(1, 2, 3))

  import cats.instances.string._
  Foldable[List].foldMap(List(1, 2, 3))(_.toString)

  import cats.instances.vector._
  val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(ints)

  import cats.syntax.foldable._

  List(1, 2, 3).combineAll
  List(1, 2, 3).foldMap(_.toString)
}
