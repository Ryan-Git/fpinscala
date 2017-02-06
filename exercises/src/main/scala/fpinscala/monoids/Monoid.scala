package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._

import scala.collection.immutable.Nil
import scala.language.higherKinds
import scala.util.Right

object MonoidTest extends App {

  import fpinscala.monoids.Monoid._

  assert(wordsMonoid.op("Hic", wordsMonoid.op("est ", "chorda ")) == "Hic est chorda")
  assert(wordsMonoid.op(wordsMonoid.op("Hic ", " est"), "chorda") == "Hic est chorda")
  assert(ordered(Vector(1, 2, 3, 4, 5)))
  assert(ordered(Vector(1, 5)))
  assert(ordered(Vector(1, 3, 5)))
  assert(!ordered(Vector(1, 3, 5, 0)))
  assert(!ordered(Vector(3, 5, 0)))
  assert(!ordered(Vector(3, 5, 0, 10)))
  assert(!ordered(Vector(3, 5, 0, -10)))

  assert(frequencyMap(Vector("a rose", "is a", "rose is", "a rose")) == Map("a" -> 3, "rose" -> 3, "is" -> 2))
}

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero: String = ""
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    val zero: Nil.type = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {

    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {

    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {

    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: None.type = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {

    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2

    override def zero: (A) => A = identity
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero: A = m.zero
  }

  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen)(a => m.op(m.zero, a) == a) &&
      forAll(gen)(a => m.op(a, m.zero) == a) &&
      forAll(
        for {
          a <- gen
          b <- gen
          c <- gen
        } yield (a, b, c))(t => m.op(m.op(t._1, t._2), t._3) == m.op(t._1, m.op(t._2, t._3)))

  val wordsMonoid: Monoid[String] = new Monoid[String] {

    override def op(a1: String, a2: String): String = if (a1.endsWith(" ")) a1 + a2.trim else a1 + " " + a2.trim

    override def zero: String = ""
  }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.isEmpty) m.zero
    else if (as.length == 1) f(as.head)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean = {

    val orderedMonoid = new Monoid[(Int, Int, Boolean)] {

      override def op(a1: (Int, Int, Boolean), a2: (Int, Int, Boolean)): (Int, Int, Boolean) = {
        if (a1._3 && a2._3 && a1._2 <= a2._1) (a1._1, a2._2, true)
        else (Int.MinValue, Int.MaxValue, false)
      }

      override def zero: (Int, Int, Boolean) = (Int.MinValue, Int.MinValue, true)
    }

    foldMapV(ints, orderedMonoid)(i => (i, i, true))._3
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {

    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
      case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
      case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
    }

    override val zero: WC = Stub("")
  }

  def count(s: String): Int = {

    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    def unstub(s: String) = s.length min 1

    foldMap(s.toList, wcMonoid)(wc) match {
      case Stub(c) => unstub(c)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }

  }


  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    sys.error("todo")

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    sys.error("todo")

  implicit def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {

    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

    override def zero: (A, B) = (A.zero, B.zero)
  }

  implicit def coproductMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[Either[A,B]] = new Monoid[Either[A, B]] {

    override def op(a1: Either[A, B], a2: Either[A, B]): Either[A, B] = (a1, a2) match {
      case (Left(l1), Left(l2)) => Left(A.op(l1, l2))
      case (Right(r1), Right(r2)) => Right(B.op(r1, r2))
      case _ => zero
    }

    override def zero: Either[A, B] = Right(B.zero)
  }

  implicit def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {

    override def op(f: (A) => B, g: (A) => B): (A) => B = a => B.op(f(a), g(a))

    override def zero: (A) => B = _ => B.zero
  }

  implicit def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {

    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = a1.keySet.union(a2.keySet).map(k => {
      (k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
    }).toMap

    override def zero: Map[K, V] = Map()
  }

  def frequencyMap(strings: IndexedSeq[String]): Map[String, Int] = {

    val mergeCount: Monoid[Map[String, Int]] = mapMergeMonoid(intAddition)

    foldMapV(strings, mergeCount)(_.split(" ").map((_, 1)).toMap)
  }

  def monoid[A](implicit A: Monoid[A]): Monoid[A] = A

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldLeft(as)(List[A]())(_ :+ _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = {
    import Monoid.foldMapV
    foldMapV(as, mb)(f)
  }

}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Some(a) => f(a)
    case None => mb.zero
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
}