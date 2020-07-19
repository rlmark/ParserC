import Parser.Parser
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class ParserSpec extends AnyFlatSpecLike with Matchers {
  "const" should "succeed returning value without consuming any input" in {
    val value = 1
    val input = "1"
    Parser.const(value)(input) shouldBe List((value, input))
  }

  it should "should handle empty input" in {
    val value = 1
    Parser.const(value)("") shouldBe List((value, ""))
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
    val parser1: Parser[String] = Parser.const("one")
    val parser2: Parser[String] = Parser.const("two")

    // Combine the two parsers into a new parser, and then we feed it input
    Parser.sequence(parser1, parser2)("input") shouldBe List((("one", "two"), "input"))
  }

  it should "handle when one parser is the zero parser" in {
    val parser1: Parser[String] = Parser.const("one")
    val parser2: Parser[String] = Parser.zero

    Parser.sequence(parser1, parser2)("input") shouldBe List()
  }

  "bind" should "chain parser results" in {
    val parser1: Parser[String] = Parser.const("one")
    val parser2: Parser[String] = Parser.const("two")
    val seqParser: Parser[(String, String)] =  Parser.sequence(parser1, parser2)
    def f(tuple: (String, String)): Parser[String] = Parser.const[String](tuple._1.toUpperCase)

    Parser.bind(seqParser)(f)("input") shouldBe List(("ONE", "input"))
  }

  it should "chain zero parser results" in {
    val parser1: Parser[String] = Parser.zero
    val parser2: Parser[String] = Parser.const("two")
    val seqParser: Parser[(String, String)] =  Parser.sequence(parser1, parser2)

    def f(tuple: (String, String)): Parser[String] = Parser.const[String](tuple._2.toUpperCase)

    Parser.bind(seqParser)(f)("input") shouldBe List()
  }
}
