package com.zdx.cats.part2.testingAsynchronousCode
import cats.Id


/**
  * Created by zhoudunxiong on 2019/6/13.
  */
class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {

  def getUpTime(hostname: String): Int =
    hosts.getOrElse(hostname, 0)
}
