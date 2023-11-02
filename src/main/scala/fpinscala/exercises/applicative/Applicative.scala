package fpinscala.exercises.applicative

import fpinscala.answers.monads.Functor
import fpinscala.answers.monoids.Monoid
import fpinscala.answers.state.State
import fpinscala.exercises.monads.Id

trait Applicative[F[_]] extends Functor[F]:
  self =>

  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    fa.map2(fab)((a, fn) => fn(a))

  extension [A](fa: F[A])
    def map2[B,C](fb: F[B])(f: (A, B) => C): F[C] =
      val intermediate: F[B => C] = apply(unit(f.curried))(fa)
      apply(intermediate)(fb)

    def map[B](f: A => B): F[B] =
      apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B])) ((a, acc) => f(a).map2(acc)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    fa.map(List.fill(n))

  extension [A](fa: F[A])
    def product[B](fb: F[B]): F[(A, B)] =
      fa.map2(fb)((_, _))

    def map3[B, C, D](
      fb: F[B],
      fc: F[C]
    )(f: (A, B, C) => D): F[D] =
      val intermediateB = apply(unit(f.curried))(fa)
      val intermediateC = apply(intermediateB)(fb)
      apply(intermediateC)(fc)

    def map4[B, C, D, E](
      fb: F[B],
      fc: F[C],
      fd: F[D]
    )(f: (A, B, C, D) => E): F[E] =
      val intermediateB = apply(unit(f.curried))(fa)
      val intermediateC = apply(intermediateB)(fb)
      val intermediateD = apply(intermediateC)(fc)
      apply(intermediateD)(fd)

  def product[G[_]](G: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] = new:
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs(0))(p(0)), G.apply(fs(1))(p(1)))

  def compose[G[_]](G: Applicative[G]): Applicative[[x] =>> F[G[x]]] = new:
      def unit[A](a: => A) = self.unit(G.unit(a))
      extension[A] (fga: F[G[A]])
        override def map2[B, C](fgb: F[G[B]])(f: (A, B) => C) =
          self.map2(fga)(fgb)(G.map2(_)(_)(f))


  def sequenceMap[K,V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map.empty[K, V])) { case ((k, fv), acc: F[Map[K, V]]) =>
      acc.map2(fv) { case (map, v) =>
        map.updated(k, v)
      }
    }

object Applicative:
  opaque type ZipList[+A] = LazyList[A]

  object ZipList:
    def fromLazyList[A](la: LazyList[A]): ZipList[A] = la
    extension [A](za: ZipList[A]) def toLazyList: LazyList[A] = za

    given zipListApplicative: Applicative[ZipList] with
      def unit[A](a: => A): ZipList[A] =
        LazyList.continually(a)
      extension [A](fa: ZipList[A])
        override def map2[B, C](fb: ZipList[B])(f: (A, B) => C) =
          fa.zip(fb).map(f.tupled)

  enum Validated[+E, +A]:
    case Valid(get: A) extends Validated[Nothing, A]
    case Invalid(error: E) extends Validated[E, Nothing]
  
  object Validated:
    given validatedApplicative[E: Monoid]: Applicative[Validated[E, _]] with
      def unit[A](a: => A) = Valid(a)
      extension [A](fa: Validated[E, A])
        override def map2[B, C](fb: Validated[E, B])(f: (A, B) => C) =
          (fa, fb) match
            case (Invalid(ea), Invalid(eb)) => Invalid(implicitly[Monoid[E]].combine(ea, eb))
            case (Invalid(ea), _) => Invalid(ea)
            case (_, Invalid(eb)) => Invalid(eb)
            case (Valid(a), Valid(b)) => Valid(f(a,b))

  type Const[A, B] = A

  object IdStuff:
    given idApplicative: Applicative[Id] with
      def unit[A](a: => A) = Id(a)

  given monoidApplicative[M](using m: Monoid[M]): Applicative[Const[M, _]] with
    def unit[A](a: => A): M = m.empty
    override def apply[A, B](m1: M)(m2: M): M = m.combine(m1, m2)

  given optionMonad: Monad[Option] with
    def unit[A](a: => A): Option[A] = Some(a)
    extension [A](oa: Option[A])
      override def flatMap[B](f: A => Option[B]) = oa.flatMap(f)

  given eitherMonad[E]: Monad[Either[E, _]] with
    def unit[A](a: => A): Either[E, A] = Right(a)
    extension [A](eea: Either[E, A])
      override def flatMap[B](f: A => Either[E, B]) = eea match
        case Left(e) => Left(e)
        case Right(a) => f(a)

  given stateMonad[S]: Monad[State[S, _]] with
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    extension [A](st: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(st)(f)
