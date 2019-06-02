package com.zdx.cats.part1.functor

import cats.Functor

/**
  * Created by zhoudunxiong on 2019/6/2.
  */
trait Tree[+A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class Leaf[A](value: A) extends Tree[A]

object Tree {

  def leaf[A](value: A): Tree[A] = Leaf(value)

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {

    def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }
}
