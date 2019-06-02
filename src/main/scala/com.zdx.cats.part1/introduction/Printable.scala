package com.zdx.cats.part1.introduction

/**
  * Created by zhoudunxiong on 2019/6/1.
  */
trait Printable[A] {

  def format(value: A): String
}

object Printable {

  def format[A](value: A)(implicit pa: Printable[A]): String = {
    pa.format(value)
  }

  def print[A](value: A)(implicit pa: Printable[A]): Unit = {
    println(format(value))
  }
}

object PrintableInstance {

  implicit val stringPrintable = new Printable[String] {
    def format(value: String): String = value.toString
  }

  implicit val intPrintable = new Printable[Int] {
    def format(value: Int): String = value.toString
  }


}


object PrintableSyntax {

  implicit class PrintableOps[A](value: A) {

    def format(implicit pa: Printable[A]): String =
      Printable.format(value)

    def print(implicit pa: Printable[A]): Unit =
      Printable.print(value)
  }
}
