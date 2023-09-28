package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  lazy val intAddition: Monoid[Int] = new Monoid[Int]:
    override def combine(a1: Int, a2: Int): Int = a1 + a2

    override def empty: Int = 0

  lazy val intMultiplication: Monoid[Int] = new Monoid[Int]:
    override def combine(a1: Int, a2: Int): Int = a1 * a2

    override def empty: Int = 1

  lazy val booleanOr: Monoid[Boolean] = new Monoid[Boolean]:
    override def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def empty: Boolean = false

  lazy val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]:
    override def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def empty: Boolean = true

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]:
    override def combine(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def empty: Option[A] = None

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A]:
    override def combine(a1: A => A, a2: A => A): A => A = a1 andThen a2

    override def empty: A => A = identity

  import fpinscala.exercises.testing.{Prop, Gen}
   import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val associativity = Prop.forAll(gen ** gen ** gen) { case a1 ** a2 ** a3 =>
      m.combine(a1, m.combine(a2, a3)) == m.combine(m.combine(a1, a2), a3)
    }
    val empty = Prop.forAll(gen) { input =>
      m.combine(input, m.empty) == input
    }
    associativity && empty

  def combineAll[A](as: List[A], m: Monoid[A]): A = as match
    case head :: Nil => head
    case head :: tail => m.combine(head, combineAll(tail, m))
    case Nil => m.empty

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    combineAll(as.map(f), m)

  def foldRight[FRA, FRB](as: List[FRA])(acc: FRB)(f: (FRA, FRB) => FRB): FRB =
    as match
      case Nil => acc
      case head :: tail =>
        val test: FRA => FRB => FRB = f.curried
        foldMap(as, endoMonoid)(f.curried)(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    val temp = (a: A) => (b: B) => f(b, a)
    foldMap(as, endoMonoid)(temp)(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.size match
      case n if n > 1 =>
        val (left, right) = as.splitAt(n/2)
        m.combine(foldMapV(left, m)(f), foldMapV(right, m)(f))
      case 1 => f(as.head)
      case _ => m.empty

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    new Monoid[Par[A]]:
      override def combine(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.combine)

      override def empty: Par[A] = Par.unit(m.empty)

  // This type checks but need to actually fork or something
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    val p: Monoid[Par[B]] = par(m)
    foldMapV(v, p)(f.andThen(Par.unit))

  def ordered(ints: IndexedSeq[Int]): Boolean =
    ???

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)) = ???
    val empty = ???

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = ???
    val empty: A => B = a => ???

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) = ???
    val empty = ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???

end Monoid
