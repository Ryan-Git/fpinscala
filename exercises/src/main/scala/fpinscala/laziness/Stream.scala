package fpinscala.laziness

import scala.annotation.tailrec

trait Stream[+A] {

  import fpinscala.laziness.Stream._

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = sys.error("todo")

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, s) => if (p(a)) cons(a, s) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = sys.error("todo")

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, s) => cons(f(a), s))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, s) => if (f(a)) cons(a, s) else s)

  def append[B >: A](s: Stream[B]): Stream[B] = this.foldRight(s)((aa, t) => cons(aa, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, s) => f(a).append(s))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold(this) {
    case Cons(h, t) if n > 1 => Some((h(), t().takeViaUnfold(n - 1)))
    case Cons(h, _) if n == 1 => Some((h(), empty))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, other)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zip[B](other: Stream[B]): Stream[(A, B)] = zipWith(other)((_, _))

  def zipAllWith[B, C](other: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = unfold((this, other)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some(f(Some(h1()), None), (t1(), empty))
    case (Empty, Cons(h2, t2)) => Some(f(None, Some(h2())), (empty, t2()))
    case (Empty, Empty) => None
  }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = zipAllWith(other)((_, _))

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(_._2.isDefined).forAll { case (o1, o2) => o1 == o2 }

  def tails: Stream[Stream[A]] = unfold(this) {
    case state@Cons(_, t) => Some(state, t())
    case _ => None
  }.append(Stream(empty))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, b) => {
      lazy val bb = b
      val r = f(a, bb._1)
      (r, cons(r, bb._2))
    })._2

  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), acc :+ h())
    }

    go(this, Nil)
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val stream: Stream[A] = cons(a, stream)
    stream
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Long] = {
    def go(i: Long, j: Long): Stream[Long] = cons(i, go(j, i + j))

    go(0, 1)
  }

  def fibsViaUnfold(): Stream[Long] = unfold((0l, 1l))(s => Some((s._1, (s._2, s._1 + s._2))))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(s => Some(a, a))

  def onesViaUnfold(): Stream[Int] = unfold(1)(s => Some(s, s))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map(r => cons[A](r._1, unfold[A, S](r._2)(f))).getOrElse(empty)

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails.exists(_ startsWith s2)

  def main(args: Array[String]): Unit = {
    val con = Stream(1, 2, 3)
    println(s"toList: ${con.toList}")
    println(s"take(2).toList: ${con.take(2).toList}")
    println(s"fibs(): ${fibs().take(10).toList}")
    println(s"fibsViaUnfold(): ${fibsViaUnfold().take(10).toList}")
    println(s"from(5): ${from(5).take(10).toList}")
    println(s"fromViaUnfold(5): ${fromViaUnfold(5).take(10).toList}")
    println(s"tails: ${con.tails.toList.map(_.toList)}")
    println(s"scanRight: ${con.scanRight(0)(_ + _).toList}")
  }
}