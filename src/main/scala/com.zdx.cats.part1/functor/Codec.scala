package com.zdx.cats.part1.functor


/**
  * Created by zhoudunxiong on 2019/6/2.
  */
trait Codec[A] {

  def encode(value: A): String

  def decode(value: String): A

  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    val self = this
    new Codec[B] {

      def encode(value: B): String =
        self.encode(enc(value))

      def decode(value: String): B =
        dec(self.decode(value))
    }
  }
}

object Codec {

  def encode[A](value: A)(implicit ca: Codec[A]): String =
    ca.encode(value)

  def decode[A](value: String)(implicit ca: Codec[A]): A =
    ca.decode(value)

  implicit val stringCodec: Codec[String] = new Codec[String] {
    def encode(value: String): String = value

    def decode(value: String): String = value
  }

  implicit val intCodec: Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  implicit val booleanCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)


  implicit def boxCodec[A](implicit ca: Codec[A]): Codec[Box[A]] =
    ca.imap(Box(_), _.value)
}
