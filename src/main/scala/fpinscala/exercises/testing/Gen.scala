package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import fpinscala.answers.testing.Prop
import fpinscala.exercises.state.RNG.{double, nonNegativeInt}
import fpinscala.exercises.testing.Result.Passed

import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/
//opaque type State[S, +A] = S => (A, S)
opaque type Gen[+A] = State[RNG, A]

object Gen:
  def choose(start: Int, stopExclusive: Int): Gen[Int] = State(
    RNG.until((i: Int) => i >= start && i < stopExclusive)(_.nextInt)
  )
  def boolean: Gen[Boolean] = State(RNG.map(RNG.int){ i => if(i %2 == 0) true else false})
  def double: Gen[Double] = State(RNG.double)

  def unit[A](a: => A): Gen[A] = State(RNG.unit(a))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if(_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    State(RNG.double).flatMap(d => if d < g1Threshold then g1._1 else g2._1)
  extension [A](self: Gen[A])
    // We should use a different method name to avoid looping (not 'run')
    def next(rng: RNG): (A, RNG) = self.run(rng)
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMapCurried(self)(f)

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(listOfN(_))

    def listOfN(n: Int): Gen[List[A]] =
      val generators: List[Gen[A]] = List.fill(n)(self)
      State.sequence(generators)



opaque type MaxSize = Int
opaque type TestCases = Int
object TestCases:
  extension (x: TestCases) def toInt: Int = x
  def fromInt(x: Int): TestCases = x

opaque type FailedCase = String

object FailedCase:
  extension (f: FailedCase) def string: String = f

  def fromString(s: String): FailedCase = s

opaque type SuccessCount = Int

object SuccessCount:
  extension (x: SuccessCount) def toInt: Int = x

  def fromInt(x: Int): SuccessCount = x

opaque type Prop = ( TestCases, RNG) => Result

enum Result:
  case Passed
  case Falsified(failure: FailedCase, successCount: SuccessCount)

  def isFalsified: Boolean = this match
    case Passed => false
    case Falsified(_, _) => true


object Prop:
  def apply(f: (TestCases, RNG) => Result): Prop =
    (n, rng) => f(n, rng)
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = (n, rng) =>
      randomLazyList(gen)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map:
          case (a, i) =>
            try
              if f(a) then Passed else Result.Falsified(a.toString, i)
            catch
              case e: Exception =>
                Result.Falsified(buildMsg(a, e), i)
        .find(_.isFalsified)
        .getOrElse(Passed)

  def randomLazyList[A](g:Gen[A])(rng: RNG): LazyList[A]=
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n generated an exception: ${e.getMessage}\n stack trace: ${e.getStackTrace.mkString}"
  extension (self: Prop)
    def check(
               maxSize: MaxSize = 100,
               testCases: TestCases = 100,
               rng: RNG = RNG.Simple(System.currentTimeMillis)
             ): Result =
      self(testCases, rng)

    def &&(that: Prop): Prop = (cases, rng) =>
      val selfResult = self(cases, rng)
      if(selfResult.isFalsified)
        selfResult
      else
        that(cases, rng)

    def || (that: Prop): Prop = (cases, rng) =>
      val selfResult = self(cases, rng)
      if (selfResult.isFalsified)
        that(cases, rng)
      else
        selfResult


/*object Gen:
  def unit[A](a: => A): Gen[A] = ???

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait Gen[A]:
  def map[B](f: A => B): Gen[B] = ???
  def flatMap[B](f: A => Gen[B]): Gen[B] = ???
 */

trait SGen[+A]
