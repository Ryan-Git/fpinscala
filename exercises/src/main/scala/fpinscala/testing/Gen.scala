package fpinscala.testing

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.laziness.Stream
import fpinscala.parallelism.Par
import fpinscala.state._
import fpinscala.testing.Gen._
import fpinscala.testing.Prop._

import scala.language.postfixOps

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/
case object Main extends App {

  import Prop._
  import SGen._

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(SGen.listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }

  Prop.run(maxProp)

  val sortedProp = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    l.isEmpty || !l.zip(l.tail).exists { case (a,b) => a > b }
  }

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

  val p2 = check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  val p3 = check {
    Par.equal (
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    ) (ES) get
  }
}

sealed trait Status

case object Exhausted extends Status

case object Proven extends Status

case object Unfalsified extends Status

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (maxSize, testCases, rng) =>
      this.run(maxSize, testCases, rng) match {
        case Right(_) => p.run(maxSize, testCases, rng)
        case l => l
      }
  }

  def ||(p: Prop) = Prop {
    (maxSize, n, rng) =>
      run(maxSize, n, rng) match {
        // In case of failure, run the other prop.
        case Left(msg) => p.tag(msg).run(maxSize, n, rng)
        case x => x
      }
  }

  def tag(msg: String) = Prop {
    (maxSize, n, rng) =>
      run(maxSize, n, rng) match {
        case Left(e) => Left(msg + "\n" + e)
        case x => x
      }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type Result = Either[FailedCase, (Status, SuccessCount)]

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      def go(i: Int, j: Int, s: Stream[Option[A]], onEnd: Int => Result): Result =
        if (i == j) Right((Unfalsified, i))
        else s.uncons match {
          case Some((Some(h), t)) =>
            try {
              if (f(h)) go(i + 1, j, s, onEnd) else Left(h.toString)
            }
            catch {
              case e: Exception => Left(buildMsg(h, e))
            }
          case Some((None, _)) => Right((Unfalsified, i))
          case None => onEnd(i)
        }

      def randomStream(a: Gen[A])(rng: RNG): Stream[A] = {
        val (v, r2) = a.sample.apply(rng)
        Stream.cons(v, randomStream(a)(r2))
      }

      go(0, n / 3, a.exhaustive, i => Right((Proven, i))) match {
        case Right((Unfalsified, _)) =>
          val rands = randomStream(a)(rng).map(Some(_))
          go(n / 3, n, rands, i => Right((Unfalsified, i)))
        case s => s
      }
    }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = g match {
    case Sized(forSize) => forAll(forSize)(f)
    case UnSized(gen) => forAll(gen)(f)
  }


  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = n / max + 1
      val props: List[Prop] = Stream.from(0).take(max + 1).map(i => forAll(g(i))(f)).toList
      props.map(p => Prop((max, n, rng) => p.run(max, casesPerSize, rng))).reduceLeft(_ && _).run(max, n, rng)
  }

  def check(p: => Boolean): Prop = forAll(unit())(_ => p)

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Left(msg) => println("! test failed:\n" + msg)
      case Right((Unfalsified, n)) =>
        println("+ property unfalsified, ran " + n + " tests")
      case Right((Proven, n)) =>
        println("+ property proven, ran " + n + " tests")
      case Right((Exhausted, n)) =>
        println("+ property unfalsified up to max size, ran " +
          n + " tests")
    }
  }
}

import fpinscala.state.RNG
import fpinscala.state.RNG.Rand

case class Gen[+A](sample: Rand[A], exhaustive: Stream[Option[A]]) {

  def map[B](f: A => B): Gen[B] = Gen(RNG.map(sample)(f), exhaustive.map(_.map(f)))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(RNG.map2(this.sample, g.sample)(f), exhaustive.map2(g.exhaustive)((oa, ob) => oa.flatMap(a => ob.map(b => f(a, b)))))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(RNG.flatMap(sample)(f(_).sample), exhaustive.flatMap {
      case Some(a) => f(a).exhaustive
      case _ => Stream(None)
    })

  /** Generate lists of length n, using the given generator. */
  def listOfN(n: Int): Gen[List[A]] =
    Gen[List[A]](
      RNG.sequence(List.fill(n)(sample)),
      Stream[Option[List[A]]] {
        val l = exhaustive.toList.filter(_.isDefined).map(_.get)
        if (l.nonEmpty) Some(l)
        else None
      }
    )

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(listOfN)

  /**
    * EXERCISE 9 (hard, optional): Try implementing the version of listOfN which picks its size up front purely in terms of other primitives. Is it possible? If yes, give an implementation. Otherwise, explain why not:*
    * it's impossible cause we don't have any primitives take A that returns List[A]
    */

  def union[B >: A](g2: Gen[B]): Gen[B] =
    boolean.flatMap(if (_) this else g2)

  def unSized: UnSized[A] = UnSized(this)
}

object Gen {

  def unit[A](a: A): Gen[A] = Gen(RNG.unit(a), Stream(Some(a)))

  def boolean: Gen[Boolean] = Gen(RNG.boolean, Stream(true, false).map(Some(_)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG.map(RNG.int)(i => i + i % (stopExclusive - start)),
      Stream(start until stopExclusive: _*).map(Some(_))
    )

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = for {
    i <- choose(from, to)
    j <- choose(from, to)
  } yield if (i % 2 == j % 2) (i, j) else (if (i + 1 == to) i - 1 else i + 1, j)


  /** Between 0 and 1, not including 1. */
  def uniform: Gen[Double] = Gen(RNG.doubleViaMap, Stream(None))

  /** Between `i` and `j`, not including `j`. */
  def choose(i: Double, j: Double): Gen[Double] = Gen(RNG.map(RNG.doubleViaMap)(d => d + d % (j - i)), Stream(None))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    if (g1._2 > g2._2) weighted(g2, g1)
    else {
      val ratio = g2._2 / g1._2
      choose(0d, ratio).flatMap(d => if (d > 1) g2._1 else g1._1)
    }

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] = g.map(i => _ => i)

  def genFn[A, B](in: Rand[A])(out: Rand[B]): Gen[A => B] = Gen[A => B](
    (rng) => {
      val (a, r1) = in(rng)
      val (b, r2) = out(r1)
      (a => b, r2)
    }, Stream(None)
  )
}

sealed trait SGen[+A] {

  def map[B](f: A => B): SGen[B]

  def flatMap[B](f: A => SGen[B]): SGen[B]

  def map2[B, C](g: SGen[B])(f: (A, B) => C): SGen[C] = for {
    i <- this
    j <- g
  } yield f(i, j)

}

case class Sized[+A](forSize: Int => Gen[A]) extends SGen[A] {

  override def map[B](f: A => B): SGen[B] = Sized(size => this.forSize(size).map(f))

  override def flatMap[B](f: (A) => SGen[B]): SGen[B] = Sized(size => {
    forSize(size).flatMap(a => {
      f(a) match {
        case Sized(r) => r(size)
        case UnSized(r) => r
      }
    })
  })
}

case class UnSized[+A](get: Gen[A]) extends SGen[A] {

  override def map[B](f: (A) => B): SGen[B] = get.map(f).unSized

  override def flatMap[B](f: (A) => SGen[B]): SGen[B] = Sized(size => {
    get.flatMap(a => {
      f(a) match {
        case Sized(r) => r(size)
        case UnSized(r) => r
      }
    })
  })
}

object SGen {

  def listOf[A](g: Gen[A]): Sized[List[A]] = Sized(size => g.listOfN(size))

  def listOf1[A](g: Gen[A]): Sized[List[A]] = Sized(size => g.listOfN(size.max(1)))

}

