package com.zdx.cats.part1.monad

/**
  * Created by zhoudunxiong on 2019/6/4.
  */
object ReaderMonad {

  import cats.data.Reader

  case class Cat(name: String, favoriteFood: String)

  val catName = Reader[Cat, String](_.name)

  def readFunc[A, B](f: A => B): A => B = a => f(a)

  val greetingKitty = catName.map(name => s"Hello $name")

  val feedKitty = Reader[Cat, String]("have a " + _.favoriteFood)

  val greetAndFeed: Reader[Cat, String] = for {
    greet <- greetingKitty
    feed <- feedKitty
  } yield s"$greet, $feed"

  greetAndFeed(Cat("zdx", "fish"))

  case class Db(usernames: Map[Int, String],
                passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(
    _.usernames.get(userId)
  )

  def checkPassword(username: String,
                    password: String): DbReader[Boolean] = Reader {
    _.passwords.get(username).contains(password)
  }

  import cats.syntax.applicative._

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      usernameOP <- findUsername(userId)
      passwordOk <- usernameOP.map(username => checkPassword(username, password)).getOrElse(false.pure[DbReader])
    } yield passwordOk

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )
  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)
  checkLogin(1, "zerocool").run(db)

  checkLogin(4, "davinci").run(db)
}
