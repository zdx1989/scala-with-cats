package com.zdx.cats.part2.testingAsynchronousCode

import cats.syntax.traverse._
import cats.syntax.functor._
import cats.instances.list._
import cats.Applicative

import scala.language.higherKinds

/**
  * Created by zhoudunxiong on 2019/6/13.
  */
class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {

  def getTotalTime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUpTime).map(_.sum)


}
