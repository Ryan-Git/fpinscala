package fpinscala.iomonad

/**
  * Created by hongdi.ren.
  */
trait Trampoline[+A] {

  def map[B](f: A => B): Trampoline[B] =
    this flatMap(a => Done(f(a)))

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = Bind(() => this, f)
}

case class Done[+A](get: A) extends Trampoline[A]

case class More[+A](force: () => Trampoline[A]) extends Trampoline[A]

case class Bind[A, +B](force: () => Trampoline[A],
                       f: A => Trampoline[B]) extends Trampoline[B]

object Trampoline {

  @annotation.tailrec
  final def run[A](t: Trampoline[A]): A = t match {
    case Done(get) => get
    case More(force) => run(force())
    case Bind(force, f) => force() match {
      case Done(a) => run(f(a))
      case More(innerMoreForce) => run(f(innerMoreForce()))
      case Bind(innerBindForce, g) => run(innerBindForce().flatMap(a => g(a) flatMap f))
    }
  }

}