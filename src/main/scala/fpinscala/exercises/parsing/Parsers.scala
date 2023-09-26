package fpinscala.exercises.parsing

import fpinscala.exercises.testing.*

import java.util.regex.Pattern
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
    def many: Parser[List[A]] = map2(p.many)(_ :: _) | succeed(Nil)
    def many1: Parser[List[A]] = map2(p.many)(_ :: _)
    def map[B](f: A => B): Parser[B] = flatMap(x => succeed(f(x)))
    def product[B](p2: => Parser[B]): Parser[(A, B)] = flatMap(a => p2.map(b => (a, b)))
    def **[B](p2: Parser[B]): Parser[(A, B)] = product(p2)
    def listOfN(n: Int): Parser[List[A]] = if (n > 0) map2(p.listOfN(n-1))(_ :: _) else succeed(Nil)
    def flatMap[B](f: A => Parser[B]): Parser[B]
    def map2[B, C](p2: =>Parser[B])(f: (A, B) => C): Parser[C]=
      product(p2).map(f.tupled)

    def slice: Parser[String]
    def attempt: Parser[A]

    def combo[B](p2: => Parser[B]) = p.slice.map2(p2)((_, b) => b)
    def *>[B](p2: => Parser[B]) = p.slice.map2(p2)((_, b) => b)
    def <*(p2: => Parser[Any]) = p.map2(p2.slice)((a, b) => a)
    
    

   

    def as[B](b: B): Parser[B] = p.slice.map(_ => b)


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
