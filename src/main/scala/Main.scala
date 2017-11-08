object Main extends App {
  println("Hello, World!")
}

object Matcher {

  def toPhrases(text: String)(implicit corpus: Corpus): Seq[String] =
    ???


}

sealed case class Corpus(phrases: Seq[String]) {
  val words = phrases map { _.split("\\s+") }

  def matches(phrase: String): Boolean = {
    val w = phrase split("\\s+")
    words.exists(_ sameElements w)
  }

}
