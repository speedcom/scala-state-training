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
}

object SoftwareCornerApp extends App {
  import SoftwareCornerState._

  val text = "To jest przykladowy tekst ktory ma ciekawy kontekst i mozliwe ze jakies slowa sie w nim powtarzaja albo i nie"

  val wordsArray = words(text)

  val wordCountMap = wordCounts(text)

  println(wordCountMap)

}