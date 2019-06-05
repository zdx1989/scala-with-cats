package com.zdx.cats.part1.monad

/**
  * Created by zhoudunxiong on 2019/6/5.
  */
object CustomMonad {

  import cats.Monad

  val optionMonad = new Monad[Option] {

    def pure[A](x: A): Option[A] = Some(x)

    def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] =
      fa.flatMap(f)

    //make sure stack safe
    def tailRecM[A, B](a: A)(f: (A) => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None => None
        case Some(Right(rb)) => Some(rb)
        case Some(Left(ra)) => tailRecM(ra)(f)
      }

  }

  sealed trait Tree[A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  //producing a kind of “growing” or “feathering” behaviour
  implicit val treeMonad = new Monad[Tree] {
    def pure[A](x: A): Tree[A] = leaf(x)

    def flatMap[A, B](fa: Tree[A])(f: (A) => Tree[B]): Tree[B] =
      fa match {
        case Leaf(value) => f(value)
        case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
      }

    def tailRecM[A, B](a: A)(f: (A) => Tree[Either[A, B]]): Tree[B] = {
      flatMap(f(a)) {
        case Left(value) =>
          tailRecM(value)(f)
        case Right(value) =>
          leaf(value)
      }
    }

  }

  import cats.syntax.functor._
  import cats.syntax.flatMap._

  val tree1 =
    branch(leaf(100), leaf(200)).flatMap { x =>
      branch(leaf(x - 1), leaf(x + 1))
    }

  val tree2 = for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c
}
