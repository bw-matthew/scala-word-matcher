object Main extends App {
  println("Hello, World!")
}

object Matcher {

  def toPhrases(text: String)(implicit corpus: Corpus): Seq[String] =
    toWords(text split "\\s+", 5) map { _.phrase } filter { _.isDefined } map { _.get } map { _ mkString " " }

  private def toWords(words: Seq[String], window: Int)(implicit corpus: Corpus): Seq[Match] =
    if (window <= 0)
      Seq(Match(None, Some(words)))
    else
      toWords(words, window - 1)

}

sealed case class Match(phrase: Option[Seq[String]], remainder: Option[Seq[String]])

sealed case class Corpus(phrases: Seq[String]) {
  val words = phrases map { _.split("\\s+") }

  def matches(phrase: Seq[String]): Boolean =
    words.exists(_ sameElements phrase)

}
