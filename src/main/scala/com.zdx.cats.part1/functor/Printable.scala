package com.zdx.cats.part1.functor

/**
  * Created by zhoudunxiong on 2019/6/2.
  */
trait Printable[A] {
  self =>

  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String = {
        self.format(func(value))
      }
    }
}

object Printable {

  def format[A](value: A)(implicit pa: Printable[A]): String =
    pa.format(value)

  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(value: String): String =
      "/" + value + "/"
  }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    def format(value: Boolean): String =
      if (value) "Yes" else "No"
  }

  implicit def boxPrintable[A](implicit pa: Printable[A]): Printable[Box[A]] = new Printable[Box[A]] {
    override def format(ba: Box[A]): String = {
      pa.format(ba.value)
    }
  }
}

final case class Box[A](value: A)