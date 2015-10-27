package fpinscala.errorhandling

import fpinscala.UnitSpec
import org.scalatest._

/**
 * Created by howard.fackrell on 10/23/15.
 */
class EitherTest extends UnitSpec {

  "Either" should " map a right to another right" in {
    assert(Right(2).map(x => x*2) === Right(4))
  }

  it should " map a Left to another Left" in {
    assert(Right(2).map(x => x*2) === Right(4))
  }

  it should "flatMap a Right" in {
    assert(Right(2).flatMap(x => Right(x*2)) === Right(4))
  }

  it should "flatMap a Left to a Left" in {
    val l : Either[String, Int] = Left("no good")
    assert(l.flatMap[String, Int](x => Right(x*2)) === Left("no good"))
  }

  it should "use the default when orElse() is used on a Left" in {
    assert( Left("no good").orElse(Right(5)) === Right(5))
  }

  it should "not use the default when orElse() is used on a Right" in {
    assert( Right(5).orElse(Right(6)) === Right(5))
  }

  it should "combine 2 Eithers into one using a function" in {
    val a = Right("my favorite number is ")
    val b = Right(3)

    assert( a.map2(b)( _+_ ) === Right("my favorite number is 3"))
  }

  it should "traverse a non empty list without errors" in {
    var l = List(1, 2, 3)

    var converted: Either[String, List[String]] = Either.traverseViaFold(l){ a =>
      if (a > 0)
        Right(2*a+"")
      else
        Left(s"$a is negative")
    }

    assert( converted === Right(List("2","4","6")) )
  }

  it should "traverse a list with errors" in {
    var l = List(1, -2, 3)

    var converted: Either[String, List[String]] = Either.traverseViaFold(l){ a =>
      if (a > 0)
        Right(2*a+"")
      else
        Left(s"$a is negative")
    }

    assert( converted === Left("-2 is negative") )
  }

  it should "sequence a non empty list without errors" in {
    var sequenced = Either.sequence( List(Right(1), Right(2), Right(3)) )
    assert( sequenced === Right(List(1,2,3)))
  }

  it should "sequence a non empty list with Left" in {
    var sequenced = Either.sequence( List(Right(1), Left("oops"), Right(3)) )
    assert( sequenced === Left("oops"))
  }



}
