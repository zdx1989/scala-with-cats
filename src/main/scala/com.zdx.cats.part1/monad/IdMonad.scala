package com.zdx.cats.part1.monad

/**
  * Created by zhoudunxiong on 2019/6/2.
  */
object IdMonad {

  import cats.Id

  def pure[A](value: A): Id[A] = value

  def map[A, B](ia: Id[A])(func: A => B): Id[B] =
    func(ia)

  def flaMap[A, B](ia: Id[A])(func: A => Id[B]): Id[B] =
    func(ia)
}
