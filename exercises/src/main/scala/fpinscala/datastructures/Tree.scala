package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {



  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  /*
  We're using the method `max` that exists on all `Int` values rather than an explicit `if` expression.


  Note how similar the implementation is to `size`. We'll abstract out the common pattern in a later exercise.
  */
  def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l, r) => max(maximum(l), maximum(r))
  }
  def max(a: Int, b : Int): Int = if(a > b) a else b

  /*
  Again, note how similar the implementation is to `size` and `maximum`.
  */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => max(1+depth(l), 1+depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /*
  Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type, and recursively
  accumulates some value using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use
  this function to implement just about any recursive function that would otherwise be defined by pattern matching.
  */
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g( fold(l)(f)(g), fold(r)(f)(g) )
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)( (a) => a)(max)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l, r) => max(l+1, r+1))

  /*
  Note the type annotation required on the expression `Leaf(f(a))`. Without this annotation, we get an error like this:

  type mismatch;
    found   : fpinscala.datastructures.Branch[B]
    required: fpinscala.datastructures.Leaf[B]
       fold(t)(a => Leaf(f(a)))(Branch(_,_))
                                      ^

  This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the
  annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument
  to `fold` will return `Leaf[B]`, which it doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to
  infer `Tree[B]` as the result type in both cases. When working with algebraic data types in Scala, it's somewhat
  common to define helper functions that simply call the corresponding data constructors but give the less specific
  result type:
  */
    def leaf[A](a: A): Tree[A] = Leaf(a)
    def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(v=> leaf(f(v))){ (l, r) =>
      Branch(l, r)
    }
  }

}

object TreeTest {



  def check[A](failureMessage : String, actual: A, expected: A): Unit = {
    if (actual != expected) {
      println(s"$failureMessage: $actual not equal to $expected ")
    }
  }

  def main(args: Array[String]) : Unit = {
    import Tree._

    check("size", size(Leaf(1)), 1)
    check("size", size( Branch(Leaf(1), Leaf(2))), 2)

    check("maximum", maximum(Leaf(4)), 4)
    check("maximum", maximum( Branch(Leaf(2), Leaf(3)) ), 3)
    check("maximum", maximum( Branch(Leaf(2), Branch(Leaf(3), Leaf(1))) ), 3)

    check("depth", depth( Leaf(4) ), 1)
    check("depth", depth( Branch( Leaf(4), Branch( Leaf(2), Leaf(3) ) )), 3)

    check("map", map( Branch( Leaf(1), Leaf(2)))(_*2), Branch( Leaf(2), Leaf(4)))

    check("sizeViaFold", sizeViaFold( Leaf(1) ), 1)

    check("sizeViaFold", sizeViaFold( Branch(Leaf(1), Leaf(2)) ),  2)

    check("depthViaFold", depthViaFold( Leaf(4) ), 1)
    check("depthViaFold", depthViaFold( Branch( Leaf(4), Branch( Leaf(2), Leaf(3) ) )), 3)

    check("mapViaFold", mapViaFold( Branch( Leaf(1), Leaf(2)))(_*2), Branch( Leaf(2), Leaf(4)))

  }
}