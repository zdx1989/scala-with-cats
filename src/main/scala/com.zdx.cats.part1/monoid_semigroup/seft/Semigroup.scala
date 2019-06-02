package com.zdx.cats.part1.monoid_semigroup.seft

/**
  * Created by zhoudunxiong on 2019/6/1.
  */
trait Semigroup[A] {

  def compile(x: A, y:A): A
}

trait Monoid[A] extends Semigroup[A] {

  val empty: A
}

object Monoid {

  def apply[A](implicit ma: Monoid[A]): Monoid[A] = ma


  // and operator &&
  implicit val booleanAndMonoid = new Monoid[Boolean] {
    val empty: Boolean = true

    def compile(x: Boolean, y: Boolean): Boolean = x && y
  }

  // or operate ||
  implicit val booleanOrMonoid = new Monoid[Boolean] {
    val empty: Boolean = false

    def compile(x: Boolean, y: Boolean): Boolean = x || y
  }

  // union set
  implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    val empty: Set[A] = Set()

    def compile(x: Set[A], y: Set[A]): Set[A] = x union y
  }

}

