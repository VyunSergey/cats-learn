package examples.databasereader

final case class DataBase(
                         usernames: Map[Int, User],
                         passwords: Map[User, Password]
                         )
