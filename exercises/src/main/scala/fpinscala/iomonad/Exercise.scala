package fpinscala.iomonad

import scala.annotation.tailrec

/**
  * Created by hongdi.ren.
  */
object Exercise {

  trait IO[F[_], +A]

  case class Pure[F[_], +A](get: A) extends IO[F, A]

  case class Request[F[_], I, +A](expr: F[I], receive: I => IO[F, A]) extends IO[F, A]

  trait Run[F[_]] {
    def apply[A](expr: F[A]): (A, Run[F])
  }

  object IO {

    @tailrec
    def run[F[_], A](R: Run[F])(io: IO[F, A]): A = io match {
      case Pure(a) => a
      case Request(expr, receive) => R(expr) match {
        case (a, r2) => run(r2)(receive(a))
      }
    }

    def monad[F[_]] = new Monad[({type f[a] = IO[F, a]})#f] {

      override def unit[A](a: => A): IO[F, A] = Pure(a)

      override def flatMap[A, B](a: IO[F, A])(f: (A) => IO[F, B]): IO[F, B] = a match {
        case Pure(v) => f(v)
        case Request(expr, receive) => Request(expr, receive.andThen(flatMap(_)(f)))
      }
    }

  }

  trait Console[+A]

  case object ReadLine extends Console[Option[String]]

  case class PrintLine(s: String) extends Console[Unit]

//  def console(lines: List[String]): Run[Console] = new Run[Console[Option[String]]] {
//    override def apply[Option](c: Console[Option]): (Option, Run[Console]) = (c, lines) match {
//      case (ReadLine, head :: tail) => (Some(head), console(tail))
//      case _ => (None, Run())
//    }
//  }
}

