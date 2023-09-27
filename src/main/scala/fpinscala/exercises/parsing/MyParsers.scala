package fpinscala.exercises.parsing

import scala.util.matching.Regex

class MyParserF[+A]() {



}
object MyParsers extends Parsers[MyParserF] {
  type MyParser[+A] = MyParserF[A]

  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = ???


  def succeed[A](a: A): MyParser[A] = ???

  def fail(msg: String): MyParser[Nothing] = ???

  def string(w: String): MyParser[String] = ???

  def regex(r: Regex): MyParser[String] = ???



  extension[A] (p: MyParser[A]) {
    def run(input: String): Either[ParseError, A] = ???

    infix def or(p2: => MyParser[A]): MyParser[A] = ???
    def flatMap[B](f: A => MyParser[B]): MyParser[B] = ???

    def attempt: MyParser[A] = ???
    def slice: MyParser[String] = ???
  }


}
