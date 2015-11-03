package fpinscala

import fpinscala.laziness.Empty

/**
 * Created by howard.fackrell on 11/1/15.
 */
class StreamSpec extends UnitSpec {

  import fpinscala.laziness.Stream._
  import fpinscala.laziness.Stream


  def even(x: Int): Boolean = (x%2)==0

  "apply" should "create a stream with the expected elements" in {
    val s = Stream.apply(1, 2, 4)
    val l = s.toList
    assert (l === List(1, 2, 4))
  }

  "take" should "return an empty stream when called on an empty stream" in {
    val s = Stream.empty.take(4)
    assert(s === Stream.empty)
  }

  it should "return a Stream with n values if n values exist in the original Stream" in {
    val s = Stream(1,2,3,4)
    assert(s.take(3).toList === List(1,2,3))
  }

  it should "return all items in the stream if the origin stream has fewer than n items" in {
    val s = Stream(1,2,3)
    assert(s.take(4).toList === List(1,2,3))
  }

  "drop" should "return an empty stream if the origin stream is empty" in {
    assert(Empty.drop(3) === Empty)
  }

  it should "return empty if more values are dropped than were in the originial" in {
    assert(Stream(1,2,3).drop(4) === Empty )
  }

  it should "return a Stream of remaining values " in {
    assert(Stream(1,2,3).drop(2).toList === List(3))
  }

  "takeWhile" should "return an empty Stream if the original Stream is empty"  in {
    assert(Empty.takeWhile(_ => true) === Empty)
  }

  it should "return empty Stream if first element doesn't match predicate" in {
    assert(Stream(1,2,3).takeWhile(_%2 == 0) === Empty)
  }

  it should "return all elements up to the first element that doesn't match the predicate" in {
    assert(Stream(2,4,6,3).takeWhile(_%2 == 0).toList === List(2,4,6))
  }

  "forAll" should "return true if the predicate matches all values in the stream" in {
    assert( Stream(2, 4, 6).forAll( even ) === true)
  }

  it should "return false if any element doesn't match the predicate" in {
    assert( Stream(2, 4, 5, 6).forAll( even ) === false)
  }

  it should "return true for an empty Stream" in {
    val s : Stream[Int] = Stream.empty
    assert( s.forAll(even) === true )
  }

  "headOption" should "return None for an empty Stream" in {
    val s : Stream[Int] = Stream.empty
    assert(s.headOption === None)
  }

  it should "return Some(1) from Stream(1,2,3)" in {
    assert(Stream(1,2,3).headOption.get === 1)
  }

  "map" should "map values into a new straam" in {
    assert ( Stream(1,2,3).map( _*2 + "" ).toList === List("2", "4", "6"))
  }

  it should "map an empty Stream" in {
    val s :Stream[Int] = Stream.empty
    assert( s.map( _.toString ) === Stream.empty)
  }

  "filter" should "create a stream with only values that match the predicate" in {
    assert( Stream(1,2,3,4).filter(even).toList === List(2,4))
  }

  "append" should "combine 2 streams" in {
    val l = Stream(1,2,3)
    val r = Stream(4,5,6)
    assert( l.append(r).toList === List(1,2,3,4,5,6) )
  }

  it should "allow for the first stream to be empty" in {
    val l: Stream[Int] = Stream.empty
    val r = Stream(4,5,6)
    assert( l.append(r).toList === List(4,5,6) )
  }

  it should "allow for the second stream to be empty" in {
    val l = Stream(1,2,3)
    val r: Stream[Int] = Stream.empty
    assert( l.append(r).toList === List(1,2,3) )
  }

  it should "append 2 empty Streams into a single empty String" in {
    val l: Stream[Int] = Stream.empty
    val r: Stream[Int] = Stream.empty
    assert( l.append(r) === Stream.empty )
  }

  "flatmap" should "map into a single stream" in {
    val s = Stream(2,3)
    val t = s.flatmap(x => Stream(1.to(x):_*))
    assert(t.toList === List(1,2,1,2,3))
  }

  "constant" should "give a Stream of consant values" in {
    assert( Stream.constantVieUnfold("i").take(2).toList === List("i", "i") )
  }

  "from" should "give a list of incrementing integer starting at the expected value" in {
    assert( Stream.fromViaUnfold(4).take(2).toList === List(4,5))
  }

  "fibs" should "give the fibonacci sequence" in {
    assert( Stream.fibsViaUnfold.take(6).toList === List(0,1,1,2,3,5) )
  }

  "zipWith" should "combine 2 streams" in {
    val twos = Stream.constant(2)
    val s = Stream(1, 2, 4)
    assert(s.zipWith(twos)(_*_).toList === List(2,4,8))
  }

  "zipAll" should "combine 2 streams into pairs" in {
    val s = Stream(1, 2, 3)
    val t = Stream("one", "two")
    val c = s.zipAll(t)
    assert( c.toList === List( (Some(1), Some("one")), (Some(2), Some("two")), (Some(3), None) ))
  }

  "startsWith" should "return true if a starts with b" in {
    val a = Stream.from(0)
    val b = Stream(0, 1, 2, 3)
    assert (a.startsWith(b))
  }






}
