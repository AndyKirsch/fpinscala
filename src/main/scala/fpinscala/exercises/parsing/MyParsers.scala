package fpinscala.exercises.parsing

import scala.util.matching.Regex

case class MyParserF[+A](value: Either[ParseError, A]) {



}
object MyParsers extends Parsers[MyParsers.MyParser] {
  type MyParser[+A] = String => MyParserF[A]

  //def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = ???

  def success[A](a: A): MyParserF[A] = MyParserF(Right(a))
  def succeed[A](a: A): MyParser[A] = _ => success(a)

  // Need location data/to understand the error structure
  def failure(msg: String): MyParserF[Nothing] = MyParserF(Left(ParseError()))
  def fail(msg: String): MyParser[Nothing] = _ => failure(msg)

  def string(w: String): MyParser[String] = input => if (input.startsWith(w)) {
    success(w)
  } else {
    failure(s"Did not find '$w'")
  }

  def regex(r: Regex): MyParser[String] = input => if(r.matches(input)) {
    success(input)
  } else {
    failure(s"Did not match $r")
  }

  extension[A] (p: MyParser[A]) {
    def run(input: String): Either[ParseError, A] = p(input).value.swap.map(x => ParseError(otherFailures = List(x))).swap

    infix def or(p2: => MyParser[A]): MyParser[A] = input => {
      val value: Either[ParseError, A] = p(input).value
      lazy val p2Value: Either[ParseError, A] = p2(input).value
      MyParserF(value.orElse(p2Value))
    }

    def flatMap[B](f: A => MyParser[B]): MyParser[B] = input => {
      // well that ugly
      val value: Either[ParseError, MyParser[B]] = p(input).value.map(f)
      value match
        case Left(err) => MyParserF(Left(err))
        case Right(parser) => parser(input)
    }

    // still don't know what this does
    def attempt: MyParser[A] = ???
    // still don't know what this does
    def slice: MyParser[String] = ???
  }
}
