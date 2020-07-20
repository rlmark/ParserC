

object Parser {
  type Parser[A] = String => List[(A, String)]

  def pure[A](a: A): Parser[A] = input => List((a, input))

  def zero[A]: Parser[A] = _ => List()

  def item: Parser[Char] = input => {
    input.headOption.fold(List.empty[(Char, String)])(c => List((c, input.tail)))
  }

  def sequence[A, B](a: Parser[A], b: Parser[B]): Parser[(A, B)] = input => {
    a(input).flatMap {
      case (a1, input1) => b(input1).flatMap { case (b1, input2) =>
        List(((a1, b1), input2))
      }
    }
  }

  def sequenceByBind[A, B](a: Parser[A], b: Parser[B]): Parser[(A, B)] = {
    bind(a) { a1: A => bind(b)
              { b1: B => input2 =>
                List(((a1, b1), input2))
              }
    }
  }

  def bind[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B] = {
    (input: String) =>
      a(input).flatMap { case (a: A, s: String) =>
        val fa: Parser[B] = f(a)
        val b: List[(B, String)] = fa(s)
        b
      }
  }

  def satisfies(predicate: Char => Boolean): Parser[Char] = input => {
    item(input).flatMap{ case (c, i) => if (predicate(c)) List((c, i)) else List() }
  }

  def satisfiesWithBind(predicate: Char => Boolean): Parser[Char] = {
    val p1: Parser[Char] = (input: String) => item(input)
    bind(p1){char => if(predicate(char)) pure(char) else zero}
  }

  def char(character: Char): Parser[Char] = satisfies(c => c == character)

  def digit: Parser[Char] = satisfies(c => c.isDigit)

  def lower: Parser[Char] = satisfies(c => c.isLower)

  def plus[A](parser1: Parser[A], parser2: Parser[A]): Parser[A] = {
    input => parser1(input) ++ parser2(input)
  }
}