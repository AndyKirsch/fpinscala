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
import scala.annotation.targetName

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

  def gpy2: Gen[Par[Int]] =
    choose(-100, 100).listOfN(choose(0, 20)).map(nums =>
      nums.foldLeft(Par.unit(0))((p, y) =>
        Par.fork(p.map2(Par.unit(y))(_ + _))))

  extension [A](self: Gen[A])
    // We should use a different method name to avoid looping (not 'run')
    def next(rng: RNG): (A, RNG) = self.run(rng)
    def map[B](f: A => B): Gen[B] = flatMap(x => Gen.unit(f(x)))
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMapCurried(self)(f)

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(listOfN(_))

    def listOfN(n: Int): Gen[List[A]] =
      val generators: List[Gen[A]] = List.fill(n)(self)
      State.sequence(generators)

    def unsized: SGen[A] = _ => self
    def list: SGen[List[A]] = i => self.listOfN(i)

    def nonEmptyList: SGen[List[A]] = n => listOfN(n.max(1))

    def **[B](second: Gen[B]): Gen[(A, B)] = flatMap(a => second.flatMap(b => Gen.unit((a,b))))

  object `**`:
    def unapply[A, B](p: (A, B)) = Some(p)


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

opaque type Prop = (MaxSize, TestCases, RNG) => Result

enum Result:
  case Passed
  case Falsified(failure: FailedCase, successCount: SuccessCount)

  def isFalsified: Boolean = this match
    case Passed => false
    case Falsified(_, _) => true


object Prop:
  def apply(f: (MaxSize, TestCases, RNG) => Result): Prop =
    (max, n, rng) => f(max, n, rng)

    //opaque type State[S, +A] = S => (A, S)
    //opaque type Gen[+A] = State[RNG, A]
    // SGen[A] == Int => Gen[A]
    // SGen[A] == Int => State[RNG, A]
    // SGen[A] == Int => S => (A, S)
    // Prop = (Int, Int, RNG) => Result
  @targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = (max, n, rng) =>
    val casesPerSize = (n.toInt - 1) /max.toInt + 1
    val props: LazyList[Prop] = LazyList.from(0).take((n.toInt min max.toInt) + 1).map(i => forAll(g(i))(f))
    val list: List[Prop] = props.map[Prop](p => (max, n, rng) =>
      p(max, casesPerSize, rng)).toList
    val prop: Prop = list.reduce(_ && _)
    prop(max, n, rng)

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop:
    (_, n, rng) =>
      randomLazyList(as)(rng).zip(LazyList.from(0)).take(n).map:
        case (a, i) =>
          try
            if f(a) then Passed else Result.Falsified(a.toString, i)
          catch
            case e: Exception => Result.Falsified(buildMsg(a, e), i)
      .find(_.isFalsified).getOrElse(Passed)

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
      self(maxSize, testCases, rng)

    def &&(that: Prop): Prop = (maxSize, cases, rng) =>
      val selfResult = self(maxSize, cases, rng)
      if(selfResult.isFalsified)
        selfResult
      else
        that(maxSize, cases, rng)

    def || (that: Prop): Prop = (maxSize, cases, rng) =>
      val selfResult = self(maxSize, cases, rng)
      if (selfResult.isFalsified)
        that(maxSize, cases, rng)
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

opaque type SGen[+A] = Int => Gen[A]

object SGen:
  def apply[A](f: Int => Gen[A]): SGen[A] = f

  extension [A](self: SGen[A])
    def apply(n: Int): Gen[A] = self(n)
    def map[B](f: A=>B): SGen[B] = i => self(i).map(f)

    def flatMap[B](f: A => SGen[B]): SGen[B] = i => self(i).flatMap(genned => f(genned).apply(i))


