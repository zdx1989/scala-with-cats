package com.zdx.cats.part2.dataValidation

/**
  * Created by zhoudunxiong on 2019/6/15.
  */
object DataValidation {
  // combine small checks into large ones

  //the check data type, a value to a value in a context
  type check[E, A] = A => Either[E, A]



}
