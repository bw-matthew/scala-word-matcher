import org.scalatest._

class MatcherSpec extends FlatSpec with Matchers {
  "The Matcher" should "not match missing phrases" in {
    implicit val corpus = Corpus(Seq("one two three", "a b c"))
    val phrase = "the quick dog"

    Matcher toPhrases phrase shouldEqual Seq()
  }
}

class CorpusSpec extends FlatSpec with Matchers {
  "The Corpus" should "match existing phrases" in {
    val corpus = Corpus(Seq("one two three", "a b c"))
    val phrase = "one two three" split "\\s+"

    corpus matches phrase shouldEqual true
  }

  "The Corpus" should "not match missing phrases" in {
    val corpus = Corpus(Seq("one two three", "a b c"))
    val phrase = "one two three four" split "\\s+"

    corpus matches phrase shouldEqual false
  }
}
