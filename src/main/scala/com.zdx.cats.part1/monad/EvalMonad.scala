package com.zdx.cats.part1.monad

import cats.Eval

/**
  * Created by zhoudunxiong on 2019/6/4.
  */
object EvalMonad {

  val saying = Eval.always {
    println("Step 1"); "a cat"
  }.map { str =>
    println("Step 2"); s"$str sat on"
  }.memoize.map { str =>
    println("Step 3"); s"$str the mat"
  }

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  def foldRightEval[A, B](list: List[A], acc: Eval[B])(func: (A, Eval[B]) => Eval[B]): Eval[B] = {
    list match {
      case Nil => acc
      case head :: tail => Eval.defer(func(head, foldRightEval(tail, acc)(func)))
    }
  }

  def foldRight[A, B](list: List[A], acc: B)(func: (A, B) => B): B =
    foldRightEval(list, Eval.now(acc)) { (a, eb) =>
      eb.map(b => func(a, b))
    }.value

}
