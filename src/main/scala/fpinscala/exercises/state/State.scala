package fpinscala.exercises.state

import scala.annotation.tailrec


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (raw, next) = rng.nextInt
    (Math.abs(raw), next)

  def doubleOrig(rng: RNG): (Double, RNG) =
    val (raw, next) = RNG.nonNegativeInt(rng)
    val scaling = raw
    (scaling, next)

  def double(rng: RNG): (Double, RNG) = RNG.map(nonNegativeInt) { _.toDouble / Int.MaxValue }(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (int, intermediate) = rng.nextInt
    val (double, next) = RNG.double(intermediate)
    ((int, double), next)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val (int, intermediate) = rng.nextInt
    val (double, next) = RNG.double(intermediate)
    ((double, int), next)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (first, intermediate1) = RNG.double(rng)
    val (second, intermediate2) = RNG.double(intermediate1)
    val (third, next) = RNG.double(intermediate2)
    ((first, second, third), next)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0)
      (Nil, rng)
    else
      val (int, next) = rng.nextInt
      val (tail, outputRNG) = ints(count - 1)(next)
      (int:: tail, outputRNG)

  @tailrec
  def until[T](p: T => Boolean)(nextFn: RNG => (T, RNG))(rng: RNG): (T, RNG) =
    val (item, next) = nextFn(rng)
    if(p(item))
      (item, next)
    else
      until(p)(nextFn)(next)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng =>
    val (a, intermediate) = ra(rng)
    val (b, next) = rb(intermediate)
    (f(a,b), next)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight((x => (Nil, x)): Rand[List[A]]) { (a, acc) =>
      map2(a, acc) { (item, list) =>
        item :: list
      }
    }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rnd =>
    val (a, next) = r(rnd)
    f(a)(next)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = flatMap(r) {a =>
    rnd => (f(a), rnd)
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a =>
    mapViaFlatMap(rb) { b => f(a, b) }
  }

opaque type State[S, +A] = S => (A, S)

object State:
  def flatMapCurried[A, B, S](state: State[S,A])(f: A => State[S, B]): State[S, B]=
    state.flatMap(f)
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] = state =>
      val (a, next) = underlying.run(state)
      (f(a), next)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = initialState =>
        val (a, intermediate) = underlying.run(initialState)
        val (b, next) = sb.run (intermediate)
        (f(a, b), next)

    def flatMap[B](f: A => State[S, B]): State[S, B] = initial =>
      val (a, next) = underlying(initial)
      f(a)(next)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =
    rng => (a, rng)

  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    rs.foldRight((x => (Nil, x)): State[S, List[A]]) { (a, acc) =>
      a.map2(acc) { (item, list) =>
        item :: list
      }
    }

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State((machine: Machine) =>
      val finalMachine = inputs.foldLeft(machine) { (state, input) =>
        (state, input) match
          case (m @ Machine(_, 0, _), _) => m
          case (m @ Machine(true, _, _), Input.Coin) => m.copy(locked = false, coins = m.coins + 1)
          case (m @ Machine(false, _, _), Input.Turn) => m.copy(locked =  true, candies = m.candies - 1)
          case _ => state
      }
      ((finalMachine.coins, finalMachine.candies), finalMachine))
