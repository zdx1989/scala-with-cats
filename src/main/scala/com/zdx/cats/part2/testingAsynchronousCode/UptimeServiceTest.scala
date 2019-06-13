package com.zdx.cats.part2.testingAsynchronousCode

/**
  * Created by zhoudunxiong on 2019/6/13.
  */
object UptimeServiceTest extends App {

  def testTotalTime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalTime(hosts.keySet.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  testTotalTime()
}
