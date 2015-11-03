package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = toListViaFoldRight

  def toListRecursive: List[A] =  this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toListViaFoldRight: List[A] = foldRight(List(): List[A])( (a, b) => a :: b)


  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = takeViaUnfold(n)
  def takeViaMatch(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeViaUnfold(n:Int): Stream[A] = unfold( (n, this) ) {
    case (x, Cons(h, t)) if x > 0 => Some(h(), (x-1, t()))
    case _ => None
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean) = takeWhileViaUnfold(p)

  def takeWhileViaRecursion(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A]) {
      (h, t ) =>
        if (p(h))
          cons(h, t)
        else
          empty
    }
  }

  def takeWhileViaUnfold(p: A=> Boolean): Stream[A] = {
    unfold(this){
      case Cons(h, t) if(p(h())) => Some(h(), t())
      case _ => None
    }
  }

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => b && p(a))

  def headOption: Option[A] = foldRight(None: Option[A])( (a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = mapViaUnfold(f)
  def mapViaFoldRight[B](f: A => B): Stream[B] =
    foldRight(Stream.empty: Stream[B])( (h, t) => cons(f(h), t))

  def mapViaUnfold[B](f: A=> B): Stream[B] =
    unfold(this) {
        case Cons(h, t) => Some((f(h()), t()))
        case Empty => None
    }


  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty: Stream[A]) {
      (h, t) =>
        if (p(h)) cons(h, t)
        else t
    }
  }

  def append[B>:A](other : => Stream[B]): Stream[B] = {
    foldRight(other){
      (h, t) => cons(h, t)
    }
  }

  def flatmap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty: Stream[B]) {
      (h, t) => f(h).append(t)
    }
  }

  def zipWith[B,C](other: Stream[B])(f: (A, B) => C):Stream[C] = {
    unfold( (this, other) ) {
      case ( Cons(ah, at), Cons(bh, bt) ) => Some(f(ah(), bh()), (at(), bt()))
      case _ => None
    }
  }

  def zipAll[B](other:Stream[B]):Stream[(Option[A], Option[B])] = {
    unfold( (this, other) ){
      case ( Empty, Empty ) => None
      case (a, b) => Some( (a.headOption, b.headOption) , (a.drop(1), b.drop(1)) )
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    val zipped: Stream[Boolean] = this.zipWith(s){
      (a, b) => a == b
    }
    zipped.forAll(x => x)
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1,1))

  def constant[A](a : A) : Stream[A] = Stream.cons(a, constant(a))
  def constantVieUnfold[A](a: A) : Stream[A] = unfold(a)(_ => Some(a,a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fromViaUnfold(n: Int) : Stream[Int] = unfold(n)(n => Some(n, n+1))

  def fibs: Stream[Int] = {
    def ifibs(a: Int, b: Int): Stream[Int] = Stream.cons(a, ifibs(b, a+b))
    ifibs(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case _ => Stream.empty
  }

  def fibsViaUnfold: Stream[Int] = {
    unfold( (0,1) ){ p => Some( (p._1, (p._2 ,p._1 + p._2)) ) }
  }
}