package fpinscala.state

import fpinscala.UnitSpec
import fpinscala.state.RNG.{Rand, Simple}

/**
 * Created by howard.fackrell on 11/3/15.
 */
class StateSpec extends UnitSpec {

  def inRange(x: Double) = x >=0.0 && x < 1.0
  def allInRange(xs: Double*) = xs.foldRight(true)( (d, b) => b && inRange(d) )
  def noDups[A](x : A*) = {
    x.foldRight(Set[A]()){ (a:A, s:Set[A]) => s + a}.size == x.size
  }

  "nonNegativeInt" should "return only positive integers" in {
    val rng = Simple(5)
    val (a, rng2) = rng.nextInt
    val (b, rng3) = rng.nextInt
    val (c, rng4) = rng.nextInt

    assert (a > 0 && b > 0 && c > 0)
  }

  "double" should "return a randome double [0 1)" in {
    val rng = Simple(5)
    val (a, rng2) = RNG.double(rng)
    val (b, rng3) = RNG.double(rng)
    val (c, rng4) = RNG.double(rng)
    val (d, rng5) = RNG.double(rng)

    assert(
      inRange(a) &&
      inRange(b) &&
      inRange(c) &&
      inRange(d))
  }

  "noDups" should "find dups" in {
    assert(noDups(2,1,2) == false)
  }

  "noDups" should "not find dups" in {
    assert(noDups(1,2,3))
  }

  "intDouble" should "return an (Int, Double) tuple" in {
    val rng = Simple(5)
    val ((a, x), rng2) = RNG.intDouble(rng)
    val ((b, y), rng3) = RNG.intDouble(rng2)
    val ((c, z), rng4) = RNG.intDouble(rng3)

    assert(noDups(a,b,c) && noDups(x,y,z) && allInRange(x, y, z))
  }

  "doubleInt" should "return an (Double, int) tuple" in {
    val rng = Simple(5)
    val ((x, a), rng2) = RNG.doubleInt(rng)
    val ((y, b), rng3) = RNG.doubleInt(rng2)
    val ((z, c), rng4) = RNG.doubleInt(rng3)

    assert(noDups(a,b,c) && noDups(x,y,z) && allInRange(x, y, z))
  }

  "double3" should "return 3 doubles in range" in {
    val ((a,b,c), rng2) = RNG.double3(Simple(5))
    assert(noDups(a,b,c) && allInRange(a,b,c))
  }

  "ints" should "return a list of random integers of correct length" in {
    val (xs, rng) = RNG.ints(5)(Simple(5))
    assert(noDups(xs:_*))
  }

  "map" should "apply a function" in {
    val i: Rand[Int] = RNG.int
    val s: Rand[String] = RNG.map(i)(_.toString)
    val (result, rng2) = s.apply(Simple(5))
    assert(result === "1923744")
  }

  "map2" should "apply a function with 2 parameters" in {
    val i = RNG.int
    val d:Rand[Double] = RNG.double

    val f = RNG.map2(i, d) {(a, b) => "int " + a + " double " + b }
    val (result, rng2): (String, RNG) = f.apply(Simple(4))
    assert( result.toString.equalsIgnoreCase("int 1538995 double 0.1502869511722992"))
  }

  "sequence" should "work" in {
    val l: List[Rand[Int]] = List.fill(4)(RNG.int)
    val rlist : Rand[List[Int]]= RNG.sequence(l)
    val (result, rng2) = rlist(Simple(4))
    assert(result.size == 4 && noDups(result:_*))
  }








}
