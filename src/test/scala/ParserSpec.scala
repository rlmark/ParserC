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
    Parser.zero("fail")("1") shouldBe List()
  }

  "item" should "consume the first character if input is non-empty" in {
    val input = "consume"
    Parser.item(input) shouldBe List(input.head, input.tail)
  }

  it should "fail if input is empty" in {
    Parser.item("") shouldBe List()
  }
}
