package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}

/**
  * Created by hongdi.ren.
  */
object Exercise extends App {

  val a = Par.async(42 + 1)
  val S = Executors.newFixedThreadPool(1)
  println(Par.equal(S)(a, Par.fork(a)))
}


object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    UnitFuture(f(a(es).get(), b(es).get()))
  }

  def map2ViaProduct[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = map(product(a, b))(p => f(p._1, p._2))

  def fork[A](a: => Par[A]): Par[A] = es => {
    es.submit(() => a(es).get())
  }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def async[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def asyncF[A, B](f: A => B): A => Par[B] = a => async(f(a))

  def product[A, B](a: Par[A], b: Par[B]): Par[(A, B)] = es => UnitFuture((a(es).get(), b(es).get()))

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((aa, _) => f(aa))

//  def map[A, B](a: Par[A])(f: A => B): Par[B] = es => UnitFuture(f(a(es).get()))

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldLeft[Par[List[A]]](unit(List()))(map2(_, _)(_:+_))

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = {
    sequence(l.map(asyncF(f)))
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] =
    map(sequence(l.map(asyncF(a => if (f(a)) List(a) else List()))))(_.flatten)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get
}
