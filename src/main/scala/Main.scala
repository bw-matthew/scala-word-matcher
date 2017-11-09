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
    else
      toMatch(window) filter {
        case Match(phrase, _) => phrase map { corpus.matches(_) } getOrElse(false)
      }

  private def toMatch(window: Int): Option[Match] =
    if (words.length < window)
      return None
    else {
      val offset = words.length - window
      val phrase = words slice(offset, words.length)
      val remainder = words slice(0, offset)
      Some(Match.from(phrase, remainder))
    }

}

private sealed case class Match(phrase: Option[Seq[String]], remainder: Option[Seq[String]])

private object Match {

  def from(phrase: Seq[String], remainder: Seq[String]): Match =
    Match(noneIfEmpty(phrase), noneIfEmpty(remainder))

  private def noneIfEmpty[T](value: Seq[T]): Option[Seq[T]] =
    if (value.isEmpty)
      None
    else
      Some(value)

}
