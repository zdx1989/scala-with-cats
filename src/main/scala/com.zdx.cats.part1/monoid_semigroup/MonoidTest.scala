package com.zdx.cats.part1.monoid_semigroup

/**
  * Created by zhoudunxiong on 2019/6/1.
  */

//Semigroup --> def combine[A](a1 A, a1: A): A; a1 |+| a2
//Monoid extend Semigroup --> combine & empty[A](): A
//associativeLaw and identityLaw

object MonoidTest {
  import cats.Monoid
  import cats.instances.int._
  import cats.instances.double._
  import cats.syntax.semigroup._


  def add(iterms: List[Int]): Int =
    iterms.foldLeft(Monoid[Int].empty){_ |+| _}

  def add[A](iterms: List[A])(implicit ma: Monoid[A]): A =
    iterms.foldLeft(ma.empty){_ |+| _}

  implicit val orderMonoid = new Monoid[Order] {
    def empty: Order = Order(0.0, 0.0)

    def combine(x: Order, y: Order): Order = {
      val totalCost = x.totalCost |+| y.totalCost
      val quantity = x.quantity |+| y.quantity
      Order(totalCost, quantity)
    }
  }


  case class Order(totalCost: Double, quantity: Double)
}
