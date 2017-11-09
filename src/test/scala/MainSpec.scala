import org.scalatest._

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

class MatcherSpec extends FlatSpec with Matchers {
  "The Matcher" should "not match missing phrases" in {
    implicit val corpus = Corpus(Seq("one two three", "a b c"))
    val phrase = "the quick dog"

    Matcher toPhrases phrase shouldEqual Seq()
  }

  "The Matcher" should "match existing phrases" in {
    implicit val corpus = Corpus(Seq("one two three", "a b c"))
    val phrase = "a one two three"

    Matcher toPhrases phrase shouldEqual Seq("one two three")
  }

  "The Matcher" should "prefer longer phrases" in {
    implicit val corpus = Corpus(Seq("a b", "a b c"))
    val phrase = "a b c d"

    Matcher toPhrases phrase shouldEqual Seq("a b c")
  }

  "The Matcher" should "handle overlapping phrases" in {
    implicit val corpus = Corpus(Seq("donald trump", "trump is president"))
    val phrase = "donald trump is president"

    Matcher toPhrases phrase shouldEqual Seq("trump is president")
  }
}
