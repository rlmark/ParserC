trait Parser[O] {
  def const[I](i: I): Parser[O]
}

class StringParser extends Parser[String] {
  override def const[I](i: I): Parser[String] = ???
}