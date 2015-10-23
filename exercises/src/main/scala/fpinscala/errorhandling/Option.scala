package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)


  def orElse[B>:A](ob: => Option[B]): Option[B] = this map ( Some(_) ) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if (f(v)) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap { m =>
      mean (xs map { y =>
        Math.pow(y - m, 2)
      })
    }
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap { av =>
      b.map { bv =>
        f(av, bv)
      }
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a){ x => x }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => {
      for {
        hb <- f(h)
        tb <- traverse(t)(f)
      } yield {
        hb :: tb
      }
    }
  }

  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil)){ (h, t) =>
      map2(f(h), t)(_ :: _)
    }

}


object OptionTest extends App {

  def check[A](message : String, actual : A, expected : A): Unit = {
    if (actual == expected) {
      println(s"passed: $message")
    } else {
      println(s"failed: $message $actual did not equal $expected")
    }
  }

  check("test", 1, 1)

  check("traverse non empty",
    Option.traverse1( List(Some(1), Some(2), Some(4)) )(e => e map (_*2)),
    Some(List(2, 4, 8))
  )

  check (
    "traverse empty",
    Option.traverse1(List(): List[Option[Int]])(e => e map (_ * 2)),
    Some(List())
  )

  check (
    "sequence",
    Option.sequence(List(Some(1), Some(2), Some(3))),
    Some(List(1,2,3))
  )
  check(
    "mean",
    Option.mean( Seq(1, 2, 3, 4, 5, 6) ),
    Some(3.5)
  )

  check(
    "mean empty",
    Option.mean( Seq() ),
    None
  )

  check(
    "variance",
    Option.variance( Seq(1,2,3,4,5,6) ),
    Some(2.9166666666666665)
  )

  check("filter", Some(3).filter(_ % 2 == 1), Some(3))
  check("filter", Some(2).filter(_ % 2 == 1), None)
  check("filter", None.filter(x => true), None)

}