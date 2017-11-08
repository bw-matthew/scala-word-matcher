import org.scalatest._

class CorpusSpec extends FlatSpec with Matchers {
  "The Corpus" should "match existing phrases" in {
    val corpus = Corpus(Seq("one two three", "a b c"))
    val phrase = "one two three"

    corpus matches phrase shouldEqual true
  }

  "The Corpus" should "not match missing phrases" in {
    val corpus = Corpus(Seq("one two three", "a b c"))
    val phrase = "one two three four"

    corpus matches phrase shouldEqual false
  }
}
