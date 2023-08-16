package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRightOrig[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def foldRightCheating[A,B](as: List[A], acc: B, f: (A, B) => B): B = {
    foldLeft(reverse(as), acc, (b, a) => f(a,b))
  }

  def foldRight[A, B](l: List[A], z: B, f: (A, B) => B): B = {
    val acc: B => B = (b: B) => b
    foldLeft(l, acc, (acc, a) => b => acc(f(a, b)))(z)
  }

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil => sys.error("message")
    case Cons(_, t) => t

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => sys.error("message")
    case Cons(_, t) => Cons(h, t)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    l match
      case Nil => Nil
      case Cons(_, t) => n match
        case 1 => t
        case x if x >= 2 => drop(t, n - 1)
        case _ => l
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match
      case Nil => Nil
      case Cons(h, t) => if (f(h)) {
        dropWhile(t, f)
      } else l
  }

  def init[A](l: List[A]): List[A] = {
    l match
      case Nil => sys.error("message")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = {
    l match
      case Nil => 0
      case Cons(h, t) => 1 + length(t)
  }

  @tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = {
    l match
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h),f)
  }

  def theirs[A, B](as: List[A], z: B, f: (A, B) => B): B =
    List.foldLeft[A,B=>B](as, (b: B) => b, (g: B => B, a: A) => b => g(f(a, b)))(z)
  def foldRightBig[A, BPrime](as: List[A], z: BPrime, f: (A, BPrime) => BPrime): BPrime = { // Utility functions

    def acc(b: BPrime): BPrime = b

    def h(g: BPrime => BPrime, a: A): BPrime => BPrime = {
      def internal(b: BPrime): BPrime = {
        f(a, g(b))
      }

      internal
    }

    List.foldLeft[A, BPrime => BPrime](as, acc, h).apply(z)
  }
  def sumViaFoldLeft(ns: List[Int]): Int = {
    foldLeft(ns, 0, _ + _)
  }

  def productViaFoldLeft(ns: List[Double]): Double = {
    foldLeft(ns, 1.0, _ * _)
  }

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A], (acc, a) => Cons(a, acc))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r, Cons(_,_))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldLeft(l, Nil: List[A], (acc, b) => appendViaFoldRight(acc, b))
  }

  def incrementEach(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int], (a, b) => append(Cons(a + 1, Nil), b))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String], (a, b) => append(Cons(a.toString, Nil), b))

  def map[A,B](l: List[A], f: A => B): List[B] = foldRight(l, Nil: List[B], (a, b) => append(Cons(f(a), Nil), b))

  def filter[A](as: List[A], f: A => Boolean): List[A] = foldRight(as, Nil: List[A], (a, b) => {
    if (f(a))
      append(Cons(a, Nil), b)
    else
      b
  })

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = {
    concat(map(as, f))
  }

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = {
    flatMap(as, a => if(f(a)) Cons(a, Nil) else Nil)
  }

  @tailrec
  def foldPair[A, result, C](l1: List[A], l2: List[C], acc: result, f: (result, A, C) => result): result = {
    (l1, l2) match
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (Cons(h1, t1), Cons(h2, t2)) => foldPair(t1, t2, f(acc, h1, h2), f)
  }

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = {
    reverse(foldPair(a, b, Nil: List[Int], (acc, thing1, thing2) => Cons(thing1 + thing2, acc)))
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B], f: (A, B) => C): List[C] = {
    reverse(foldPair(l1, l2, Nil: List[C], (b, a, c) => Cons(f(a, c), b)))
  }

  def zipWithTuple[A, B](l1: List[A], l2: List[B]): List[(A, B)] = {
    reverse(foldPair(l1, l2, Nil: List[(A, B)], (b, a, c)=> Cons((a,c), b)))
  }

  def all[A](l: List[A], f: A => Boolean): Boolean = {
    l match
      case Nil => true
      case Cons(h, t) => if(!f(h)) false else all(t, f)
  }

  def isNoSmallerThan[A](l: List[A], n: Int): Boolean = l match
    case Nil => n == 0
    case Cons(h, t) => if (n <= 0) true else isNoSmallerThan(t, n-1)

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val size = length(sub)
    println(s"sup $sup sub $sub")
    if (isNoSmallerThan(sup, size)) {
      val check = zipWithTuple(sup, sub)
      if (all(check, _ == _))
        true
      else
        sup match {
          case Nil => false
          case Cons(_, t) => hasSubsequence(t, sub)
        }
    } else {
      false
    }
  }
