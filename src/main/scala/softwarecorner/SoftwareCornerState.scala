package softwarecorner

import scalaz._
import Scalaz._

object SoftwareCornerState {

  // method that takes a string as a parameter and returns a list of all the words within that string
  def words(str: String): Array[String] = str.split(" ")

  // method that counts the number of times each word within a string is used
  def wordCounts(str: String): Map[String, Int] = {
    words(str).foldLeft(Map[String, Int]()) { (acc, word) => {
      val count: Int = acc.getOrElse(word, 0) + 1
      acc + (word -> count)
    }}
  }

  def wordCounts(str: String, currMap: Map[String, Int]): Map[String, Int] = {
    words(str).foldLeft(currMap) { (acc, word) => {
      val count: Int = acc.getOrElse(word, 0) + 1
      acc + (word -> count)
    }}
  }
}

object Text {
  val text = "To jest przykladowy tekst ktory ma ciekawy kontekst i " +
    "mozliwe ze jakies slowa sie w nim powtarzaja albo i nie a gwarantuje ze tak" +
    "gdyz tak bedzie ciekawej ha ha ha"
}

object SoftwareCornerApp_1 extends App {
  import SoftwareCornerState._
  import Text._

  val wordsArray = words(text)
  val wordCountMap = wordCounts(text)
  println(wordCountMap)

}

object SoftwareCornerApp_2 extends App {
  import SoftwareCornerState._
  import Text._

  case class Article(headline: String, abstr: String, body: String)
  val article = Article(text, text, text)

  val map0 = Map[String, Int]()
  val map1 = wordCounts(article.headline, map0)
  val map2 = wordCounts(article.abstr,    map1)
  val map3 = wordCounts(article.body,     map2)

  println(map3)
}

// examples of using scalaz state monad
object SoftwareCornerApp_3 extends App {

  val m1 = State { s: String => (s, s.size) }

  def repeat(num: Int): State[String, Unit] = State { s: String => (s * num, ()) }

  println(m1.run("hello"))
  println(repeat(3).run("text "))

  println(m1.flatMap(repeat).run("text "))

  // GET
  val r = get[String]
       .flatMap(s0 => repeat(s0.length))
       .flatMap(_ => get)
       .map(s1 => s1.length)
       .run("init")
  println(r)

  // PUT
  val s = get[String]
       .flatMap(s0 => put(s0 * s0.length))
       .flatMap(_ => get)
       .map(s1 => s1.length)
       .run("init")
  println(s)

  // FOR-COMPREHENSION
  val t = for {
    s0 <- get[String]
    _  <- put(s0 * s0.length)
    s1 <- get
  } yield s1.length
  println(t.run("init"))

  // MODIFY
  val u = for {
    _  <- modify { s: String => s * s.length }
    s1 <- get
  } yield s1.length
  println(u.run("init"))

  // GETS - nice and clean solution (there is no obsolete states within computation)
  val v = for {
    _ <- modify { s: String => s * s.length }
    size <- gets { s: String => s.length }
  } yield size
  println(v.run("init"))

}







