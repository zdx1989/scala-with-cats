package com.zdx.cats.part2.testingAsynchronousCode

import cats.Id

import scala.concurrent.Future
import scala.language.higherKinds

/**
  * Created by zhoudunxiong on 2019/6/13.
  */
trait UptimeClient[F[_]] {

  def getUpTime(hostname: String): F[Int]
}

//trait TestUptimeClient extends UptimeClient[Id] {
//
//  def getUpTime(hostname: String): Id[Int]
//}

trait RealUptimeClient extends UptimeClient[Future] {

  def getUpTime(hostname: String): Future[Int]
}