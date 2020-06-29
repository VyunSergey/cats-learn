package examples.validated

import cats.syntax.apply._
import cats.syntax.either._

object ValidatedHTML extends App {
  case class User(name: String, age: Int)

  def getValue(key: String)(data: FormData): FailFast[String] =
    data.get(key).toRight(List(s"Error: not found $key! Got: `${data.keys.mkString(", ")}`"))

  def parseInt(key: String)(data: String): FailFast[Int] =
    Either.catchOnly[NumberFormatException](data.toInt)
      .leftMap(_ => List(s"Error: $key must be an integer! Got: `$data`"))

  def nonEmpty(key: String)(data: String): FailFast[String] =
    Right(data).ensure(List(s"Error: $key must be not empty! Got: `$data`"))(_.nonEmpty)

  def nonNegative(key: String)(data: Int): FailFast[Int] =
    Right(data).ensure(List(s"Error: $key must be not negative! Got: `$data`"))(_ >= 0)

  /**
   * readName function read `name` field from input data
   * and checks if `name` is exists
   * and not empty
   * */
  def readName(data: FormData): FailFast[String] =
    getValue("name")(data)
      .flatMap(nonEmpty("name"))

  /**
   * readAge function read `age` field from input data
   * and checks if `age` is exists
   * and not empty
   * and is integer
   * and not negative
   * */
  def readAge(data: FormData): FailFast[Int] =
    getValue("age")(data)
      .flatMap(nonEmpty("age"))
      .flatMap(parseInt("age"))
      .flatMap(nonNegative("age"))

  /**
   * readUser function read `name` and `age` fields from input data
   * and checks with `readName` and `readAge` functions
   * */
  def readUser(data: FormData): FailSlow[User] =
    (
      readName(data).toValidated,
      readAge(data).toValidated
    ).mapN(User.apply)

  val rn1 = readName(Map("name" -> "Dane Murphy"))
  val rn2 = readName(Map("name" -> ""))
  val rn3 = readName(Map())

  println("\nTesting readName function:")
  println("Test1: " + rn1)
  println("Test2: " + rn2)
  println("Test3: " + rn3)

  val ra1 = readAge(Map("age" -> "32"))
  val ra2 = readAge(Map("age" -> ""))
  val ra3 = readAge(Map("age" -> "-1"))
  val ra4 = readAge(Map())

  println("\nTesting readAge function:")
  println("Test1: " + ra1)
  println("Test2: " + ra2)
  println("Test3: " + ra3)
  println("Test4: " + ra4)

  val ru1 = readUser(Map("name" -> "Dane Murphy", "age" -> "32"))
  val ru2 = readUser(Map("name" -> "", "age" -> "32"))
  val ru3 = readUser(Map("age" -> "32"))
  val ru4 = readUser(Map("name" -> "Dane Murphy", "age" -> ""))
  val ru5 = readUser(Map("name" -> "", "age" -> ""))
  val ru6 = readUser(Map("age" -> ""))
  val ru7 = readUser(Map("name" -> "Dane Murphy", "age" -> "-1"))
  val ru8 = readUser(Map("name" -> "", "age" -> "-1"))
  val ru9 = readUser(Map("age" -> "-1"))
  val ru10 = readUser(Map("name" -> "Dane Murphy"))
  val ru11 = readUser(Map("name" -> ""))
  val ru12 = readUser(Map())
  val ru13 = readUser(Map("age" -> "32"))
  val ru14 = readUser(Map("age" -> ""))
  val ru15 = readUser(Map("age" -> "-1"))
  val ru16 = readUser(Map())

  println("\nTesting readUser function:")
  println("Test1: " + ru1)
  println("Test2: " + ru2)
  println("Test3: " + ru3)
  println("Test4: " + ru4)
  println("Test5: " + ru5)
  println("Test6: " + ru6)
  println("Test7: " + ru7)
  println("Test8: " + ru8)
  println("Test9: " + ru9)
  println("Test10: " + ru10)
  println("Test11: " + ru11)
  println("Test12: " + ru12)
  println("Test13: " + ru13)
  println("Test14: " + ru14)
  println("Test15: " + ru15)
  println("Test16: " + ru16)
}
