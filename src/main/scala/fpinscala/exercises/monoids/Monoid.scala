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

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs =>
      foldMapV(bs, par(m)) { b =>
        Par.lazyUnit(b)
      }
    }

  def parFoldMapNotReally[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    // This type checks but does it need to actually fork or something?
    val p: Monoid[Par[B]] = par(m)
    foldMapV(v, p)(f.andThen(Par.unit))

  def ordered(ints: IndexedSeq[Int]): Boolean =
    // Not how this was expected to be done
    val data = ints.sliding(2).toIndexedSeq
    foldMapV(data, booleanAnd) { case pair =>
      pair.size match
        case n if n > 1 =>
          val l = pair(0)
          val r = pair(1)
          l <= r
        case _ => true
    }

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  object WC:
    def toString(wc: WC): String = wc match
      case Part(lStub, words, rStub) => s"Part(\"$lStub\", $words, \"$rStub\")"
      case Stub(chars) => s"Stub(\"$chars\")"

  lazy val wcMonoid: Monoid[WC] = new Monoid[WC]:
    import WC.*

    override def combine(wc1: WC, wc2: WC) = (wc1, wc2) match
      case (WC.Stub(a), WC.Stub(b)) => WC.Stub(a + b)
      case (WC.Stub(a), WC.Part(l, w, r)) => WC.Part(a + l, w, r)
      case (WC.Part(l, w, r), WC.Stub(a)) => WC.Part(l, w, r + a)
      case (WC.Part(l1, w1, r1), WC.Part(l2, w2, r2)) =>
        WC.Part(l1, w1 + (if (r1 + l2).isEmpty then 0 else 1) + w2, r2)

    def combineStillWrong(a1: WC, a2: WC): WC =
      def parse(str: String): WC = Stub(str)

      def normalize(item: WC): WC = item match
        case Stub(str) =>
          val parts = ("@"+str+"@").split("((?=:|#| )|(?<=:|#| ))")// TODO this is wrong it is dropping the tailing space
          parts(0) = parts(0).drop(1)
          parts(parts.length-1) = parts.last.dropRight(1)
          println(s"normalizing ${WC.toString(item)} with parts ${parts.length} \"${parts.mkString("'", "', '", "'")}\"")

          parts.length match
            case 0 => Stub("")
            case 1 => Stub(str)
            case len => Part(parts(0), len - 2, parts.last)
        case Part(l, count, r) =>
          val lParts = l.split(' ')
          val (left, lIncrement) = lParts.length match
            case 0 => ("", 0)
            case len => (lParts(0), len - 1)
          val rParts = r.split(' ')
          val (right, rIncrement) = rParts.length match
            case 0 => ("", 0)
            case len => (rParts.last, len - 1)

          Part(left, lIncrement + count + rIncrement, right)

      val result = (a1, a2) match
        // hardcode these in
        case (l, Stub("")) => l
        case (Stub(""), r) => r
        case (Stub(l), Stub(r)) => normalize(parse(l + r))
        case (Stub(l), Part(lStub, words, rStub)) =>
          normalize(Part(l + lStub, words, rStub))
        case (Part(lStub, words, rStub), Stub(r)) =>
          normalize(Part(lStub, words, rStub + r))
        case (Part(lStub, wordsL, mrStub), Part(mlStub, wordsR, rStub)) => normalize(Part(lStub, wordsL + wordsR + count(mrStub + mlStub), rStub))

      println(s"combining ${WC.toString(a1)} with ${WC.toString(a2)} got ${WC.toString(result)} ")
      result

    def combineOld(a1: WC, a2: WC): WC =
      def count(str: String): Int = str.split(" ").map(_.trim).count(_.nonEmpty)
      def countSides(str: String): (String, Int, String) =
        val parts = str.split(" ").map(_.trim).filter(_.nonEmpty)
        val (lStub, middleRight) = parts.splitAt(1)
        val (toBeCounted, rStub) = middleRight.splitAt(middleRight.length - 1)
        (lStub.headOption.getOrElse(""), toBeCounted.length, rStub.headOption.getOrElse(""))

      def parse(string: String): WC =
        val (lStub, count, rStub) = countSides(string)
        println(s"parsed to $lStub $count $rStub")
        (lStub, count, rStub) match {
          case ("", 0, r) => Stub(r)
          case (l, 0, "") => Stub(l)
          case (_, _, _) => Part(lStub, count, rStub)
        }

      val result = (a1, a2) match {
        // hardcode these in
        case (l, Stub("")) => l
        case (Stub(""), r) => r
        case (Stub(l), Stub(r)) => parse(l + r)
        case (Stub(l), Part(lStub, words, rStub)) =>  Part(l+lStub, words, rStub)
        case (Part(lStub, words, rStub), Stub(r)) => Part(lStub, words, rStub + r)
        case (Part(lStub, wordsL, mrStub), Part(mlStub, wordsR, rStub)) => Part(lStub, wordsL + wordsR +  count(mrStub+mlStub), rStub)
      }
      println(s"combining ${WC.toString(a1)} with ${WC.toString(a2)} got ${WC.toString(result)} ")
      result

    override def empty: WC = Stub("")

  def count(s: String): Int =
    import WC.*
    val folded = foldMapV(s, wcMonoid) { s =>
      if (s.isWhitespace) Part("", 0, "") else Stub(s.toString)
    }
    println(s"folded ${WC.toString(folded)}")
    val surrounded = wcMonoid.combine(Stub(" "), wcMonoid.combine(folded, Stub(" ")))
    println(s"surrounded ${WC.toString(surrounded)}")
    def countFinal(s: String): Int = if(s.isBlank) 0 else 1
    surrounded match
      case Stub(chars) => countFinal(chars)
      case Part(lStub, words, rStub) => countFinal(lStub) + words + countFinal(rStub)

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)) = (ma.combine(x._1, y._1), mb.combine(x._2, y._2))
    val empty = (ma.empty, mb.empty)

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = a => mb.combine(f(a), g(a))
    val empty: A => B = a => mb.empty

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) = (a.keySet ++ b.keySet).foldLeft(empty): (acc, k) =>
      acc.updated(k, mv.combine(a.getOrElse(k, mv.empty), b.getOrElse(k, mv.empty)))
    val empty = Map()

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    val m = mapMergeMonoid[A, Int](using intAddition)
    as.foldLeft(m.empty){ case (b,a) =>
      m.combine(Map(a -> 1), b)
    }

end Monoid
