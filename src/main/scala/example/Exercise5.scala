package example

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def toList: List[A] = {

    @tailrec
    def toList(s: Stream[A], l: List[A]): List[A] = {
      s match {
        case Empty => l
        case Cons(h, t) => toList(t(), l :+ h())
      }
    }

    toList(this, Nil)
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }


  def take(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) => if (n != 1) Stream.cons[A](h(), t().take(n - 1)) else Stream.cons[A](h(), Stream.empty[A])
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) => if (n != 0) t().drop(n - 1) else Stream.cons[A](h(), t())
    }
  }

  def takeWhile(f: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) => if (f(h())) Stream.cons[A](h(), t().takeWhile(f)) else Empty
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Empty => true
      case Cons(h, t) => if (!p(h())) false else t().forAll(p)
    }
  }

  def takeWhileWithFold(f: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Stream.empty[A])((a: A, b) => if (f(a)) Stream.cons(a, b) else b)
  }

  def headOptionWithFold: Option[A] = {
    foldRight[Option[A]](None)((a: A, b) => Some(a))
  }


  def map[B](f: A => B): Stream[B] = {
    foldRight[Stream[B]](Stream.empty[B])((a: A, b) => Stream.cons(f(a), b))
  }


  def append[B >: A](rest: => TraversableOnce[B]): Stream[B] = {
    foldRight[Stream[B]](Stream(rest.toList: _*))((a: A, b) => Stream.cons(a, b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Stream.empty[A])((a: A, b) => if (f(a)) Stream.cons(a, b) else b)
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

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

object Exercise5 {

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def innerFibs(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a, innerFibs(b, a + b))
    }
    innerFibs(0, 1)
  }

  def unfold[S, A](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty
      case Some(s) => Stream.cons(s._1, unfold(s._2)(f))
    }
  }

  def fibsUnfold: Stream[Int] =
    Stream.cons(0,
      Stream.cons(1, unfold( (0, 1) )(s => Some((s._2 + s._1, (s._2, s._2 + s._1)))))
    )

  def constantUnfold[A](a: A): Stream[A] = unfold(Unit)(_ => Some((a, Unit) ))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s+1))


}
