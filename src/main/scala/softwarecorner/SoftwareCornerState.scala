package softwarecorner

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