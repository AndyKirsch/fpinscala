package fpinscala.exercises.parsing

import scala.util.matching.Regex

case class MyParserF[+A](value: Either[String, A]) {



}
object MyParsers extends Parsers[MyParserF] {
  type MyParser[+A] = String => MyParserF[A]

  //def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = ???

  def success[A](a: A): MyParserF[A] = MyParserF(Right(a))
  def succeed[A](a: A): MyParser[A] = _ => success(a)

  def failure(msg: String): MyParserF[Nothing] = MyParserF(Left(msg))
  def fail(msg: String): MyParser[Nothing] = _ => failure(msg)

  def string(w: String): MyParser[String] = input => if (input.startsWith(w)) {
    success(w)
  } else {
    failure(s"Did not find '$w'")
  }

  def regex(r: Regex): MyParser[String] = input => if(input.matches(r)) {
    success(input)
  } else {
    failure(s"Did not match $r")
  }

  extension[A] (p: MyParser[A]) {
    def run(input: String): Either[ParseError, A] = p(input).value.swap.map(ParseError(otherFailures = List(_))).swap

    infix def or(p2: => MyParser[A]): MyParser[A] = ???

    def flatMap[B](f: A => MyParser[B]): MyParser[B] = ???

    def attempt: MyParser[A] = ???

    def slice: MyParser[String] = ???
  }
}
