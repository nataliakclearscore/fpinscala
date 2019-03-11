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
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    (Math.abs(i) / Integer.MAX_VALUE.toDouble, r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    var curRng: RNG = rng
    var l = List[Int]()
    1.to(count).foreach(_ => {
      val (i, nextRng) = curRng.nextInt
      curRng = nextRng
      l = i :: l
    })
    (l, curRng)
  }

  def doubleViaMap: Rand[Double] = {
    map(nonNegativeInt)(_ / (Integer.MAX_VALUE.toDouble + 1))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => fs match {
      case x :: xs =>
        val (head, nextRng1) =  x(rng)
        val (tail, nextRng2) = sequence(xs)(nextRng1)
        (head :: tail, nextRng2)
      case _ => (List(), rng)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (fRes, rng2) = f(rng)
    g(fRes)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def nonNegativeLessThan2(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
        val (fRes, s2) = run(s)
        f(fRes).run(s2)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def process(input: Input): Machine = input match {
    case Coin if locked && candies > 0 =>
        Machine(false, candies, coins + 1)
    case Turn if !locked && candies > 0 =>
        Machine(true, candies - 1, coins)
    case _ =>
      this
  }
}

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(machine =>
    inputs match {
      case Nil => ((machine.coins, machine.candies), machine)
      case input :: tail =>
        val newMachine = machine.process(input)
        simulateMachine(tail).run(newMachine)
    }
  )
}
