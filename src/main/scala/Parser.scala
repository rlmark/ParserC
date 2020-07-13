object Parser {
  type Parser[A] = String => List[(A, String)]

  def const[A](a: A): Parser[A] = input => List((a, input))
}