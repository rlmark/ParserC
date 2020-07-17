object Parser {
  type Parser[A] = String => List[(A, String)]

  def const[A](a: A): Parser[A] = input => List((a, input))

  def zero[A]: Parser[A] = _ => List()

  def item: Parser[Char] = input => {
    input.headOption.fold(List.empty[(Char, String)])(c => List((c, input.tail)))
  }

  def sequence[A,B](a: Parser[A], b: Parser[B]): Parser[(A,B)] = input => {
    val List((a1, input1)) = a(input)
    val List((b1, input2)) = b(input1)
    List(((a1, b1), input2))
  }
}