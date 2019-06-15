package com.zdx.cats.part2.mapReduce

/**
  * Created by zhoudunxiong on 2019/6/15.
  */
object ParallelFoldMap {

  import cats.Monoid
  import cats.syntax.semigroup._
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global


  def foldMap[A, B: Monoid](va: Vector[A])(func: A => B): B =
    va.foldLeft(Monoid[B].empty)(_ |+| func(_))

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors()
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    val groups = values.grouped(groupSize)
    val futures = groups.map { v =>
      Future(foldMap(v)(func))
    }
    Future.sequence(futures).map { v =>
      v.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }

}
