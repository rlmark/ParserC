import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class ParserSpec extends AnyFlatSpecLike with Matchers {
  "Parser" should "have a const function" in {
    val valueT = "hello"

    new StringParser().const(valueT) shouldBe valueT
  }
}
