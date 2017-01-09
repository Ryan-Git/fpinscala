package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed)
      // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def positiveMax(n: Int): Rand[Int] =
    map(int)(i => (i / Int.MaxValue.toDouble * n).toInt.abs)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i > Int.MinValue) (i.abs, r)
    else (0, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def doubleViaMap: Rand[Double] =
    map(int)(i => (i / (Int.MaxValue.toDouble + 1)).abs)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = intDouble(rng) match {
    case ((i, d), r) => ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (i1, r1) = rng.nextInt
    (2 to count).foldLeft((List(i1), r1))((p, _) => {
      val (ii, rr) = p._2.nextInt
      (ii :: p._1, rr)
    })
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def intDoubleViaMap2: Rand[(Int, Double)] =
    map2(int, doubleViaMap)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List[A]()))(map2(_, _)(_ :+ _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  def nonNegativeIntViaFlatMap: Rand[Int] =
    flatMap(int)(i => if (i == Int.MinValue) nonNegativeIntViaFlatMap else unit(i.abs))
}

case class State[S, +A](run: S => (A, S)) {

  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S, B](s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

object State {

  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](list: List[State[S, A]]): State[S, List[A]] =
    list.foldLeft[State[S, List[A]]](unit(List[A]()))(_.map2(_)((list, a) => a +: list))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyDispenser extends App {

  import State._

  def update(input: Input): Machine => Machine = (machine: Machine) => {
    (input, machine) match {
      case (_, Machine(_, 0, _)) => machine
      case (Coin, Machine(false, _, _)) => machine
      case (Turn, Machine(true, _, _)) => machine
      case (Coin, Machine(true, candy, coin)) =>
        Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(locked = true, candy - 1, coin)
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(i => modify(update(i))))
    s <- get
  } yield (s.candies, s.coins)

  println(simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 10, 5)))
  println(simulateMachine(List(Coin, Turn, Coin)).run(Machine(true, 10, 5)))
}

object Test extends App {

  import RNG._

  val rng = Simple(System.currentTimeMillis())
  println(s"ints: ${ints(5)(rng)}")
  println(s"positiveMax(100): ${positiveMax(100)(rng)}")
  println(s"doubleViaMap: ${doubleViaMap(rng)}")
  println(s"intDoubleViaMap2: ${intDoubleViaMap2(rng)}")

}