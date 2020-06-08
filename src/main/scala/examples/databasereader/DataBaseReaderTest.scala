package examples.databasereader

object DataBaseReaderTest extends App {
  import DataBaseReader._

  val users: Map[Int, User] = Map(
    1 -> User("Dave"),
    2 -> User("Kate"),
    3 -> User("Margo")
  )

  val passwords: Map[User, Password] = Map(
    User("Dave") -> Password("zeroCool"),
    User("Kate") -> Password("qwerty"),
    User("Margo") -> Password("34F#$F432+_)13(*&d1")
  )

  val dataBase = DataBase(users, passwords)

  val id = 1
  val pass = Password("zeroCool")
  val idCheckLog = checkLogin(id, pass).run(dataBase)
  println(s"Id $id has password $pass: " + idCheckLog)

  val id2 = 3
  val pass2 = Password("$F432+_)13(")
  val idCheckLog2 = checkLogin(id2, pass2).run(dataBase)
  println(s"Id $id2 has password $pass2: " + idCheckLog2)

  val id3 = 4
  val pass3 = Password("LoveCats!")
  val idCheckLog3 = checkLogin(id3, pass3).run(dataBase)
  println(s"Id $id3 has password $pass3: " + idCheckLog3)
}
