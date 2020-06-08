package reader

import cats.data.Reader

object ReaderTest extends App {
  case class Cat(name: String, favoriteFood: String)

  val catNameReader: Reader[Cat, String] = Reader[Cat, String](_.name)
  val cat: Cat = Cat("Garfield", "lasagne")
  println("Name: " + catNameReader.run(cat))

  val greetingCat: Reader[Cat, String] = catNameReader.map(nm => s"Hello, $nm!")
  println(greetingCat.run(cat))

  val feedCat: Reader[Cat, String] =
    Reader[Cat, String](_.favoriteFood).map(fd => s"Have a nice bowl of $fd")

  val greetingAndFeed: Reader[Cat, String] = for {
    greet <- greetingCat
    feed <- feedCat
  } yield s"$greet, $feed"
  println(greetingAndFeed(cat))

}
