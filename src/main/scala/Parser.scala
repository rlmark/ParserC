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
    flatMap(a) { a1: A => flatMap(b)
              { b1: B => input2 =>
                List(((a1, b1), input2))
              }
    }
  }

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B] = {
    (input: String) =>
      a(input).flatMap { case (a: A, s: String) =>
        val fa: Parser[B] = f(a)
        val b: List[(B, String)] = fa(s)
        b
      }
  }

  def map[A,B](a: Parser[A], f: A => B): Parser[B] = {
    (input: String) =>
      a(input).map(t => (f(t._1), t._2))
  }

  def satisfies(predicate: Char => Boolean): Parser[Char] = input => {
    item(input).flatMap{ case (c, i) => if (predicate(c)) List((c, i)) else List() }
  }

  def satisfiesWithBind(predicate: Char => Boolean): Parser[Char] = {
    val p1: Parser[Char] = (input: String) => item(input)
    flatMap(p1){ char => if(predicate(char)) pure(char) else zero}
  }

  def char(character: Char): Parser[Char] = satisfies(c => c == character)

  def digit: Parser[Char] = satisfies(c => c.isDigit)

  def lower: Parser[Char] = satisfies(c => c.isLower)

  def upper: Parser[Char] = satisfies(c => c.isUpper)

  def plus[A](parser1: Parser[A], parser2: Parser[A]): Parser[A] = {
    input => parser1(input) ++ parser2(input)
  }

  def letter: Parser[Char] = plus(lower, upper)

  def alphanumeric: Parser[Char] = plus(letter, digit)

  def word: Parser[String] = ???

  //  def stringFor(target: String): Parser[String] = {
//    target match {
//      case h s_+: t =>
//        for {
//         e <- Parser.char(h)
//        } yield ()
//    }
//  }

  def string(target: => String): Parser[String] = {
    input => {
      val parser = target match {
        case h s_+: t => flatMap(char(h)) {
          c =>
            println(s"c: ${c}")
            flatMap(string(t)){
              s =>
                println(s"s: ${s}")
                println(s"p: ${h + t}")
                pure(h + t)
            }
        }
        case _ => pure("")
      }
    println(s"input: ${input}")
    parser(input)
  }
  }

  def debugString(target: => String): Parser[String] = {
    input => {
      val parser = target match {
        case h s_+: t => flatMap(char(h)) {
          c =>
            println(s"c: ${c}")
            flatMap(debugString(t)){
              s =>
                println(s"s: ${s}")
                println(s"h: ${h} t: $t")
                pure(h + t)
            }
        }
        case _ => pure("")
      }
      println(s"input: ${input}")
      parser(input)
    }
  }

  def handleTail(c: Char, t: String, input: String): (String, String) = (c.toString + string(t)(input), input)
}

object s_+: {
  def unapply(s: String): Option[(Char, String)] = s.headOption.map{ (_, s.tail) }
}