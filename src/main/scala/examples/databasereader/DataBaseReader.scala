package examples.databasereader

import cats.data.Reader
import cats.implicits._

object DataBaseReader{
  type DBReader[A] = Reader[DataBase, A]

  def findUserName(userId: Int): DBReader[Option[User]] =
    Reader[DataBase, Option[User]](_.usernames.find(_._1 == userId).map(_._2))

  def checkPassword(user: User, password: Password): DBReader[Boolean] =
    Reader[DataBase, Boolean](_.passwords.find(_._1 == user).exists(_._2 == password))

  def checkLogin(userId: Int, password: Password): DBReader[Boolean] = for {
    user <- findUserName(userId)
    check <- user.map(checkPassword(_, password)).getOrElse(false.pure[DBReader])
  } yield check
}
