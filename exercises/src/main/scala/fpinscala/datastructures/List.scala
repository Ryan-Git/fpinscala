package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((acc: List[A], a: A) => Cons(a, acc))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("there's not tail for Nil")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => throw new NoSuchElementException
      case Cons(_, t) => drop(t, n - 1)
    }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case l => l
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, i: Int) => i + 1)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {

    def g(a: A, h: B => B): B => B = {
      h compose ((b: B) => f(b, a))
    }

    foldRight(l, identity[B](_))(g)(z)
  }


  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((i: Int, _) => i + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, a) => Cons(a, acc))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldLeft(reverse(l), Nil: List[B])((b, a) => Cons(f(a), b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldLeft(reverse(l), Nil: List[A])((acc, a) => if (f(a)) Cons(a, acc) else acc)

  def concatenate[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A])(append)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    foldLeft(l, Nil: List[B])((acc, a) => append(acc, f(a)))
  }

  def ex16(l: List[Int]) = foldRight(l, Nil: List[Int])((i, acc) => Cons(i + 1, acc))

  def ex17(l: List[Double]) = foldRight(l, Nil: List[String])((i, acc) => Cons(i.toString, acc))

  def ex21[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def ex22(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, ex22(t1, t2))
  }

  def ex23[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), ex23(t1, t2)(f))
  }

  def startWith[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startWith(t1, t2)
    case _ => false
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => false
    case _ if startWith(l, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }


  def main(args: Array[String]): Unit = {
    val l = Cons(1, Cons(2, Cons(3, Nil)))
    println(foldLeft(l, "")((s, i) => s"$s $i"))
    println(foldLeft2(l, "")((s, i) => s"$s $i"))

    val a1 = Cons(1, Cons(2, Cons(3, Nil)))
    val a2 = Cons(4, Cons(5, Cons(6, Nil)))
    println(append(a1, a2))
    println(append2(a1, a2))

    val c = Cons(Cons(1, Cons(2, Nil)), Cons(Cons(3, Nil), Cons(Cons(4, Cons(5, Nil)), Nil)))
    println(concatenate(c))
    println(foldRight(c, Nil: List[Int])(append))

    println(s"ex16: ${ex16(l)}")

    val d = Cons(1.0, Cons(2.0, Cons(3.0, Nil)))
    println(s"ex17: ${ex17(d)}")

    println(s"ex18: ${map(l)(_ + 1)}")
    println(s"flatMap: ${flatMap(l)(i => List(i, i))}")
  }
}
