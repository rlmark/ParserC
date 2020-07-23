import Parser.Parser
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class ParserSpec extends AnyFlatSpecLike with Matchers {
  "pure" should "succeed returning value without consuming any input" in {
    val value = 1
    val input = "1"
    Parser.pure(value)(input) shouldBe List((value, input))
  }

  it should "should handle empty input" in {
    val value = 1
    Parser.pure(value)("") shouldBe List((value, ""))
  }

  "zero" should "fail regardless of input" in {
    Parser.zero("fail") shouldBe List()
  }

  "item" should "consume the first character if input is non-empty" in {
    val input = "consume"
    Parser.item(input) shouldBe List((input.head, input.tail))
  }

  it should "handle input with one character" in {
    val input = "c"
    Parser.item(input) shouldBe List(('c', ""))
  }

  it should "fail if input is empty" in {
    Parser.item("") shouldBe List()
  }

  "sequence" should "apply two parsers and return a pair of results" in {
    val parser1: Parser[String] = Parser.pure("one")
    val parser2: Parser[String] = Parser.pure("two")

    // Combine the two parsers into a new parser, and then we feed it input
    Parser.sequence(parser1, parser2)("input") shouldBe List((("one", "two"), "input"))
  }

  it should "handle when one parser is the zero parser" in {
    val parser1: Parser[String] = Parser.pure("one")
    val parser2: Parser[String] = Parser.zero

    Parser.sequence(parser1, parser2)("input") shouldBe List()
  }

  "sequenceByBind" should "handle sequence" in {
    val parser1: Parser[String] = Parser.pure("one")
    val parser2: Parser[String] = Parser.pure("two")

    // Combine the two parsers into a new parser, and then we feed it input
    Parser.sequenceByBind(parser1, parser2)("input") shouldBe List((("one", "two"), "input"))
  }

  it should "handle when one parser is the zero parser" in {
    val parser1: Parser[String] = Parser.pure("one")
    val parser2: Parser[String] = Parser.zero

    Parser.sequenceByBind(parser1, parser2)("input") shouldBe List()
  }

  "bind" should "chain parser results" in {
    val parser1: Parser[String] = Parser.pure("one")
    val parser2: Parser[String] = Parser.pure("two")
    val seqParser: Parser[(String, String)] =  Parser.sequence(parser1, parser2)
    def f(tuple: (String, String)): Parser[String] = Parser.pure[String](tuple._1.toUpperCase)

    Parser.bind(seqParser)(f)("input") shouldBe List(("ONE", "input"))
  }

  it should "chain zero parser results" in {
    val parser1: Parser[String] = Parser.zero
    val parser2: Parser[String] = Parser.pure("two")
    val seqParser: Parser[(String, String)] =  Parser.sequence(parser1, parser2)

    def f(tuple: (String, String)): Parser[String] = Parser.pure[String](tuple._2.toUpperCase)

    Parser.bind(seqParser)(f)("input") shouldBe List()
  }

  "satisfies" should "returns value and remaining input if predicate true" in {
    def predicate(c: Char): Boolean = c == ('c')
    Parser.satisfies(predicate)("congratulations") shouldBe List(('c',"ongratulations"))
  }

  it should "returns empty list is predicate false" in {
    def predicate(c: Char): Boolean = c == ('c')
    Parser.satisfies(predicate)("nope") shouldBe List()
  }

  "satisfiesWithBind" should "returns value and remaining input if predicate true" in {
    def predicate(c: Char): Boolean = c == ('c')
    Parser.satisfiesWithBind(predicate)("congratulations") shouldBe List(('c',"ongratulations"))
  }

  it should "returns empty list is predicate false" in {
    def predicate(c: Char): Boolean = c == ('c')
    Parser.satisfiesWithBind(predicate)("nope") shouldBe List()
  }

  "char" should "parse single character if the character matches" in {
    Parser.char('e')("enjoyment") shouldBe List(('e', "njoyment"))
  }

  it should "return an empty list if character does not match" in {
    Parser.char('e')("nope") shouldBe List()
  }

  "digit" should "parse character as digit if valid" in {
    Parser.digit("1test") shouldBe List(('1', "test"))
  }

  it should "not parse multiple character as digit if valid" in {
    Parser.digit("12test") shouldBe List(('1', "2test"))
  }

  it should "return an empty list if character is not valid digit" in {
    Parser.digit("test") shouldBe List()
  }

  "lower" should "parse character if it is lowercase" in {
    Parser.lower("lowercase") shouldBe List(('l', "owercase"))
  }

  it should "not parse character if it is lowercase" in {
    Parser.lower("Lowercase") shouldBe List()
  }

  // Note, plus succeeds if at least one parser succeeds on the same input
  "plus" should "apply two parsers to the same input and succeed if both succeed" in {
    val parser1 = Parser.char('e')
    val parser2 = Parser.lower

    Parser.plus(parser1, parser2)("eftest") shouldBe List(('e', "ftest"), ('e', "ftest"))
  }

  it should "return an empty list if both parsers fails" in {
    val parser1 = Parser.char('e')
    val parser2 = Parser.digit

    Parser.plus(parser1, parser2)("f1test") shouldBe List()
  }

  it should "return a value if the second parser succeeds" in {
    val parser1 = Parser.char('e')
    val parser2 = Parser.digit

    Parser.plus(parser1, parser2)("1test") shouldBe List(('1', "test"))
  }

  it should "return a value if the second parser fails" in {
    val parser1 = Parser.char('e')
    val parser2 = Parser.digit

    Parser.plus(parser1, parser2)("etest") shouldBe List(('e', "test"))
  }
}
