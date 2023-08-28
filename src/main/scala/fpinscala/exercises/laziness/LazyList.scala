package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.{cons, unfold}

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z



  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Empty => Empty
    case Cons(h, t) => if(n > 0) Cons(h, () => t().take(n -1)) else Empty

  def dropOrig(n: Int): LazyList[A] = this match
    case Empty => Empty
    case Cons(h, t) => if(n > 0) t().dropOrig(n-1) else this

  def drop(n: Int): LazyList[A] = this match
    case Empty => Empty
    case Cons(h, t) => if (n > 0) t().drop(n - 1) else this

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(Empty: LazyList[A]) { (a, acc) =>
      if(p(a))
        Cons(() => a, () => acc)
      else
        acc
    }

  def forAll(p: A => Boolean): Boolean = this match
    case Empty => true
    case Cons(h, t) => if(p(h())) t().forAll(p) else false

  def headOption: Option[A] = foldRight(None: Option[A]) { (a, acc) =>
    Some(a)
  }

  def map[B](f: A=>B): LazyList[B] = foldRight(Empty: LazyList[B]) ( (a,bs) =>
    Cons(() => f(a), () => bs)
  )

  def mapWrong[B](f: A => B): LazyList[B] = this match
    case Empty => Empty
    case Cons(h, t) => Cons(() => f(h()), () => t().mapWrong(f))

  def mapViaUnfold[B](f: A=>B): LazyList[B] = unfold(() => this) { state =>
    state() match
      case Empty => None
      case Cons(h, t) => Some(f(h()), t)
  }
  def takeViaUnfold(n: Int): LazyList[A] = unfold((n, () => this)) { state =>
    state._2() match
      case Empty => None
      case Cons(h, t) => if (state._1 > 0) Some(h(), (state._1 -1, t)) else None
  }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] = unfold(() => this) { state =>
    state() match
      case Empty => None
      case Cons(h, t) => if(p(h())) Some(h(), t) else None
  }
  def zipWith[B, C](bs: LazyList[B])(f: (A, B) => C): LazyList[C] = unfold(() => (this, bs)) { state =>
    state() match
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), () => (ta(), tb()))
  }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = unfold(() => (this, that)) { state =>
    state() match
      case (Empty, Empty) => None
      case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), () => (Empty, tb()))
      case (Cons(ha, ta), Empty) => Some((Some(ha()), None), () => (ta(), Empty))
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), () => (ta(), tb()))
  }

  def filter(p:A => Boolean): LazyList[A] = foldRight(Empty:LazyList[A]) { (a, as) =>
    if(p(a))
      Cons(() => a, () => as)
    else
      as
  }

  def append[B >:A](as: LazyList[B]): LazyList[B] = foldRight(as) {(a, acc) =>
    Cons(() => a, () => acc)
  }

  def flatMap[B](f: A => LazyList[B]): LazyList[B] = foldRight(Empty:LazyList[B]) { (a, bs) =>
    f(a).append(bs)
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWithMatch[B](s: LazyList[B]): Boolean = (this, s) match
    case (_, Empty) => true
    case (Cons(h, t), Cons(h2, t2)) if h() == h2() => t().startsWith(t2())
    case _ => false
  def startsWith[B](s: LazyList[B]): Boolean = zipAll(s).takeWhile(_(1).isDefined).forAll((a1, a2) => a1 == a2)

  def tails: LazyList[LazyList[A]] = unfold(() => Option(this)) {state =>
    state() match
      case None => None
      case Some(Empty) => Some(Empty, () => None)
      case Some(Cons(h, t)) => Some(Cons(h, t), () => Some(t()))
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): LazyList[B] =
    foldRight(z -> Cons(() => z, () => Empty)) { (item, acc) =>
      val (prev, list) = acc
      val b = f(item, prev)
      (b, Cons(() => b, () => list))
    }._2

  def hasSubsequence[A](l: LazyList[A]): Boolean = tails.exists(_.startsWith(l))



object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from(n+1))

  lazy val fibs: LazyList[Int] =
    def fib(x: Int, y: Int): LazyList[(Int, Int)] = cons((x, y), fib(y, x+y))
    fib(0, 1).map( (a, b) => a)
  def fibsWill(): LazyList[Int] = unfold((0, 1)) {  case (current, next) =>
    Some(current, (next, current + next))
  }
  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Empty

  lazy val fibsViaUnfold: LazyList[Int] = unfold((0, 1)) { state => Some((state._1, (state._2, state._1 + state._2))) }

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n){state => Some(state, state + 1)}

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(a){_ => Some(a, a)}

  lazy val onesViaUnfold: LazyList[Int] = unfold(1) {_ => Some(1, 1)}
