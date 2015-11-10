package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i < 0) (-i, rng2) else (i, rng2)
  }



  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue.toDouble, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i,d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), rng2) = intDouble(rng)
    ((d,i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (a, rng2) = double(rng)
    val (b, rng3) = double(rng2)
    val (c, rng4) = double(rng3)
    ((a,b,c), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val (i, rng2) = rng.nextInt
      val (rest, rng3) = ints(count-1)(rng2)
      (i :: rest, rng3)
    } else {
      (List[Int](), rng)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra){ a =>
      map(rb) { b =>
        f(a,b)
      }
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]())) { (a, l) =>
      map2(a, l){ _ :: _}
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] = {
    State(
      state => {
      val (a, newState):(A, S) = run(state)
      (f(a), newState)
    })
  }

  def mapViaFlatMap[B](f: A=>B): State[S, B] = {
    this.flatMap(a => unit(f(a)))
  }

  def map2 = map2ViaFlatMap(_)

  def map2ViaFlatMap[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap{ a =>
      sb.map{ b => f(a,b) }
    }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(
      state => {
        val (a, state1) = run(state)
        f(a).run(state1)
      }
    )
  }
}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
