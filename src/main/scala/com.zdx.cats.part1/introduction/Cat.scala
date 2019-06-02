package com.zdx.cats.part1.introduction

/**
  * Created by zhoudunxiong on 2019/6/1.
  */
final case class Cat(name: String, age: Int, color: String)

object Cat {
  import PrintableInstance._
  import cats._
  import cats.implicits._

  implicit val catPrintable = new Printable[Cat] {
    def format(cat: Cat): String = {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is $age years-old $color cat"
    }
  }

  implicit val showCat =
    Show.show[Cat] { cat =>
      val name = cat.name.show
      val age = cat.age.show
      val color = cat.color.show
      s"$name is $age year-old $color cat"
    }

  implicit val eqCat =
    Eq.instance[Cat] { (c1, c2) =>
      c1.name === c2.name &&
      c1.age === c2.age &&
      c1.color === c2.color
    }
}
