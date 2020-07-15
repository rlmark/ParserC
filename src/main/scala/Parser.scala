object Parser {
  type Parser[A] = String => List[(A, String)]

  def const[A](a: A): Parser[A] = input => List((a, input))

  def zero[A](a: A): Parser[A] = _ => List()

  def item: Parser[Char] = input => {
    input.headOption.fold(List.empty[(Char, String)])(c => List((c, input.tail)))
  }
}