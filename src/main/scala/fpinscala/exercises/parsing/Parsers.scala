package fpinscala.exercises.parsing

import fpinscala.exercises.testing.*

import scala.util.matching.Regex



trait Parsers[Parser[+_]]:
  self => // so inner classes may call methods of trait

  def succeed[A](a: A): Parser[A]
  def fail(msg: String): Parser[Nothing]

  def string(w: String): Parser[String]

  def regex(r: Regex): Parser[String]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int

  extension[A] (p: Parser[A])
    def run(input: String): Either[ParseError, A]
    infix def or(p2: => Parser[A]): Parser[A]
    def |(p2: => Parser[A]): Parser[A] = or(p2)
    def many: Parser[List[A]] = ???//p.flatMap()
    def map[B](f: A => B): Parser[B] = flatMap(x => succeed(f(x)))
    def product[B](p2: Parser[B]): Parser[(A, B)] = ???
    def **[B](p2: Parser[B]): Parser[(A, B)] = product(p2)
    def listOfN(n: Int): Parser[List[A]] = ???
    def flatMap[B](f: A => Parser[B]): Parser[B]

  case class ParserOps[A](p: Parser[A])

  val numA: Parser[Int] = char('a').many.map(_.size)

  object Laws:
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = Prop.forAll(in)(s => p1.run(s) == p2.run(s))
    def mapLaw[A](p:Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)





case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  def remaining: String = ???

  def slice(n: Int) = ???

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.linesIterator.drop(line-1).next()
    else ""

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()):
  def push(loc: Location, msg: String): ParseError = ???

  def label(s: String): ParseError = ???

class Examples[Parser[+_]](P: Parsers[Parser]):
  import P.*

  val nonNegativeInt: Parser[Int] = ???

  val nConsecutiveAs: Parser[Int] = ???
