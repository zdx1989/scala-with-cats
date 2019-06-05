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

  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = {
    sym match {
      case "+" => operate(_ + _)
      case "_" => operate(_ - _)
      case "*" => operate(_ * _)
      case "/" => operate(_ / _)
      case num => operand(num.toInt)
    }
  }

  def operand(num: Int): CalcState[Int] = State[List[Int], Int] { state =>
    (num :: state, num)
  }

  def operate(func: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] {
    case b :: a :: tail =>
      val ans = func(a, b)
      (ans :: tail, ans)
    case _ =>
      sys.error("Fail!")
  }

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  program.run(Nil).value


  import cats.syntax.applicative._
  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState]){ (a, b) =>
      a.flatMap(_ => evalOne(b))
    }
  }

  val program1 = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value
}
