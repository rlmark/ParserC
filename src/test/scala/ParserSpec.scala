import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class ParserSpec extends AnyFlatSpecLike with Matchers {
  "Parser" should "have a const function" in {
    val int = 1
    val input = "1"

    Parser.const(int)(input) shouldBe List((int, input))
  }
}
