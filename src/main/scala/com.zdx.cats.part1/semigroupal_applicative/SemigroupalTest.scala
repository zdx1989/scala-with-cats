package com.zdx.cats.part1.semigroupal_applicative


/**
  * Created by zhoudunxiong on 2019/6/6.
  */

// Semigroup tupled and mapN
// Validated
object SemigroupalTest extends App {

  import cats.syntax.either._
  import cats.instances.option._
  import cats.instances.list._
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def parseNumber(str: String): Either[String, Int] =
    Either.catchOnly[NumberFormatException](str.toInt)
        .leftMap(_ => s"can't read $str")

  // We want all the errors but the result only contains zhe first error
  val result = for {
    a <- parseNumber("a")
    b <- parseNumber("b")
    c <- parseNumber("c")
  } yield a + b + c

  //semigroup join two values but Semigroupal join two contexts
  import cats.Semigroupal
  Semigroupal[Option].product(Some(1), Some("zdx"))

  //if either parameter evaluate is None, the final result is None
  Semigroupal[Option].product(Some(1), None)
  Semigroupal[Option].product(None, Some("zdx"))

  //join three or more contexts use tuple2 ~ tuple22
  Semigroupal.tuple3(Option(1), Option(2), Option(3))
  Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])

  //apply user-specified function use map2 ~ map22
  Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
  Semigroupal.map3(Option(1), Option.empty[Int], Option(3))(_ + _ + _)

  //apply syntax
  import cats.instances.option._
  import cats.syntax.apply._

  (Option(123), Option("abc")).tupled

  case class Cat(name: String, born: Int, color: String)

    (
      Option("zdx"),
      Option(3),
      Option("yellow")
    ).mapN(Cat.apply)

  //fancy and apply syntax

  case class Cat1(name: String, yearOfBirth: Int, favouriteFood: List[String])

  val tuple2Cat: (String, Int, List[String]) => Cat1 = Cat1.apply _

  val cat2Tuple: Cat1 => (String, Int, List[String]) = cat => (cat.name, cat.yearOfBirth, cat.favouriteFood)

  import cats.Monoid
  import cats.syntax.monoid._
  import cats.syntax.semigroupal._
  import cats.instances.string._
  import cats.instances.int._
  import cats.instances.list._
  import cats.instances.invariant._
  import cats.syntax.apply._

  implicit val catMonoid: Monoid[Cat1] = (
      Monoid[String],
      Monoid[Int],
      Monoid[List[String]]
    ).imapN(tuple2Cat)(cat2Tuple)

  val zdx = Cat1("zdx", 1999, List("fish"))

  val ygy = Cat1("ygy", 1992, List("cookie"))

  val newCat = zdx |+| ygy

  //Semigroupal applied to different types

  //Future
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import cats.instances.future._
  import cats.Semigroupal

  val future1 = Semigroupal[Future].product(
    Future("zdx"),
    Future(123)
  )

  val result1 = Await.result(future1, 3.seconds)

  println(result1)

  //We can use apply syntax to zip fixed number of Futures
  val futureCats = (
      Future("zdx"),
      Future(1990),
      Future(List("fish"))
    ).mapN(Cat1.apply)

  val resultCats = Await.result(futureCats, 2.seconds)

  println(resultCats)

  //List
  import cats.Semigroupal
  import cats.instances.list._

  val listTuple = Semigroupal[List].product(List(1, 2), List(3, 4))
  println(listTuple)

  //Either
  import cats.instances.either._

  type ErrorOr[A] = Either[Vector[String], A]

  val errorTuple = Semigroupal[ErrorOr].product(
    Left(Vector("Error1")),
    Left(Vector("Error2"))
  )

  //Semigroupal applied to Monad

  import cats.Monad
  import scala.language.higherKinds

  def product[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  //validate
  import cats.Semigroupal
  import cats.data.Validated

  type ErrorAll[A] = Validated[List[String], A]

  val errorAll = Semigroupal[ErrorAll].product(
    Validated.Invalid(List("Error1")),
    Validated.Invalid(List("Error2"))
  )

  val v = Validated.Valid(123)
  val i = Validated.Invalid(List("zdx"))

  val vv = Validated.valid[List[String], Int](123)
  val ii = Validated.invalid[List[String], Int](List("Error1"))

  import cats.syntax.validated._
  val vvv = 123.valid[List[String]]
  val iii = List("Error").invalid[Int]

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  val v4 = 123.pure[ErrorAll]
  val i4 = List("Error").raiseError[ErrorAll, Int]

  // create Validate instance from anther type
  Validated.catchOnly[NumberFormatException]("zdx".toInt)

  Validated.catchNonFatal(sys.error("Error"))

  Validated.fromTry(scala.util.Try("zdx".toInt))

  Validated.fromEither[String, Int](Left("Error"))

  Validated.fromOption[String, Int](Option(123), "ygy")

  //exercise from validation

  import cats.data.Validated

  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(field: String)(data: FormData): FailFast[String] =
    data.get(field).toRight(List(s"$field field not specified"))


  def noBlank(field: String)(data: String): FailFast[String] =
    Right(data)
      .ensure(List(s"$field cannot be blank"))(_.nonEmpty)

  def noNegative(field: String)(data: Int): FailFast[Int] =
    Right(data)
      .ensure(List(s"$field cannot be negative"))(_ > 0)

  import scala.util._

  def parseInt(field: String)(data: String): FailFast[Int] =
    Try(data.toInt) match {
      case Success(value) => Right(value)
      case Failure(_) => Left(List(s"$field must be a integer"))
    }

  def readName(map: Map[String, String]): FailFast[String] =
    getValue("name")(map)
      .flatMap(noBlank("name"))


  def readAge(map: Map[String, String]): FailFast[Int] =
    getValue("age")(map)
      .flatMap(noBlank("age"))
      .flatMap(parseInt("age"))
      .flatMap(noNegative("age"))


  case class User(name: String, age: Int)

  def readUser(map: Map[String, String]): FailSlow[User] = {
    import cats.instances.list._
    import cats.syntax.apply._
    Semigroupal.map2(
      readName(map).toValidated,
      readAge(map).toValidated
    )(User.apply)
  }
}
