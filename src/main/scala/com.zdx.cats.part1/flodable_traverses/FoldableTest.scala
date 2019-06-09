package com.zdx.cats.part1.flodable_traverses

/**
  * Created by zhoudunxiong on 2019/6/9.
  */
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

  listSequence(List(Vector(1, 2), Vector(3, 4)))
}
