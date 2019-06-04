package com.zdx.cats.part1.monad

/**
  * Created by zhoudunxiong on 2019/6/4.
  */
object StateMonad {

  // represent functions of type S => (S, A)
  import cats.data.State

  val a = State[Int, String] { state =>
    (state, s"the state is $state")
  }

  //transform a state to anther state
  //computes a result

  //get the state and the result with run function
  val (state, result) = a.run(10).value

  //get the state with runS function
  val state1 = a.runS(10).value

  //get the result with runR function
  val result1 = a.runA(10).value

  //composing and transforming State
  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1 is: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2 is: $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val (s, r) = both.run(20).value

  //get extract the State as the Result
  val getDemo = State.get[Int]
  val getResult = getDemo.run(10).value

  //set update the State and return unit as the Result
  val setDemo = State.set[Int](20)
  val (setState, setResult) = setDemo.run(10).value

  //prue ignores the State and return a supplied result
  val pureDemo = State.pure[Int, String]("Result")
  val pureResult = pureDemo.run(10).value

  //inspect extract the state via a transformation function
  val inspectDemo = State.inspect[Int, String](_ + "!")
  val (inspectState, inspectResult) = inspectDemo.run(10).value

  //modify update the state using a update function
  val modifyDemo = State.modify[Int](_ + 1)
  val (modifyState, _) = modifyDemo.run(10).value

  import State._

  val forDemo = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  val (state2, result2) = forDemo.run(1).value

  //

}
