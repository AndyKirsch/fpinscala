package fpinscala.exercises.monoids

trait Foldable[F[_]]:
  import Monoid.{endoMonoid, dual}

  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      as.foldMap(f.curried)(using endoMonoid[B])(acc)

    def foldLeft[B](acc: B)(f: (B, A) => B): B =
      as.foldMap(a => b => f(b, a))(using endoMonoid[B])(acc)

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      as.foldLeft(mb.empty){ case (b, a) => mb.combine(b, f(a))}

    def combineAll(using ma: Monoid[A]): A =
      foldRight(ma.empty)(ma.combine)

    def toList: List[A] =
      foldRight(List.empty[A]){ case (a, b) => a:: b}

object Foldable:

  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def toList: List[A] =
        as

  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        as.map(f).fold(mb.empty)(mb.combine)

  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)

  import fpinscala.exercises.datastructures.Tree

  given Foldable[Tree] with
    import Tree.{Leaf, Branch}
    extension [A](as: Tree[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as match
          case Leaf(i) => f(i, acc)
          case Branch(l, r) =>
            val right = r.foldRight(acc)(f)
            l.foldRight(right)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as match
          case Leaf(i) => f(acc, i)
          case Branch(l, r) =>
            val left = l.foldLeft(acc)(f)
            r.foldLeft(left)(f)

      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        as.fold(f, mb.combine)


  given Foldable[Option] with
    extension [A](as: Option[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as match
          case None => acc
          case Some(x) => f(x, acc)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as match
          case None => acc
          case Some(x) => f(acc, x)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        as.map(f).getOrElse(mb.empty)
