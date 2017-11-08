object Main extends App {
  println("Hello, World!")
}

sealed case class Corpus(phrases: Seq[String]) {
  val words = phrases map { _.split("\\s+") }

  def matches(phrase: Seq[String]): Boolean =
    words.exists(_ sameElements phrase)

}

object Matcher {

  def toPhrases(text: String)(implicit corpus: Corpus): Seq[String] =
    toWords(text split "\\s+", 5)
      .map { _.phrase }
      .filter { _.isDefined }
      .map { _.get }
      .map { _ mkString " " }

  private def toWords(words: Seq[String], window: Int)(implicit corpus: Corpus): Seq[Match] =
    if (window <= 0)
      Seq(Match(None, Some(words)))
    else
      Seq()
}

private sealed case class Accumulator(words: Seq[String]) {

  def accumulate(word: String): Accumulator =
    Accumulator(words :+ word)

  def convert(window: Int)(implicit corpus: Corpus): Option[Match] =
    if (words.length < window)
      return None
    else {
      val phrase = words slice(words.length - window, words.length) // you've tested everything but this window

      if (! corpus.matches(phrase))
        return None

      if (words.length > window)
        Some(Match(Some(phrase), Some(words slice(0, words.length - window))))
      else
        Some(Match(Some(phrase), None))
    }

}

private sealed case class Match(phrase: Option[Seq[String]], remainder: Option[Seq[String]])
