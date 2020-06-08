package examples.databasereader

final case class Password(password: String) {
  override def toString: String = "\"" + password + "\""
}
