package fpinscala.parsing

import fpinscala.testing.{Gen, Prop}

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

trait Parsers[Parser[+ _]] {
  self =>
  // so inner classes may call methods of trait

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(a => succeed(f(a)))

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)


  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a, b)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  implicit def string(s: String): Parser[String]

  implicit def ops[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def seq[U,A,B](f: U => Parser[A])(g: A => Parser[B]): U => Parser[B] =
    u => flatMap(f(u))(g)

  def ex8(): Parser[String] = "[0-9]+".r.flatMap(i => listOfN(i.toInt, char('a'))).slice

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {

    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = product(p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def attempt: Parser[A] = self.attempt(p)
  }

  object Laws {

    import Prop._

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => Parsers.this.run(p1)(s) == Parsers.this.run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop = forAll(in)(s => Parsers.this.run(succeed(a))(s) == Right(a))
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}