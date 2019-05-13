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

  def mapUnfold[B](f: A => B): Stream[B] = {
    Exercise5.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }
  }

  def takeUnfold(n: Int): Stream[A] = {
    Exercise5.unfold((this, n)) {
      case (Empty, _) => None
      case (_, 0) => None
      case (Cons(h, t), v) => Some((h(), (t(), v - 1)))
    }
  }

  def takeWhileUnfold(f: A => Boolean): Stream[A] = {
    Exercise5.unfold(this) {
      case Empty => None
      case Cons(h, t) => if (f(h())) Some((h(), t())) else None
    }
  }

  def zipWith[B >: A](s: Stream[B])(f: (B, B) => B): Stream[B] = {
    Exercise5.unfold(this, s) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    Exercise5.unfold(this, s2) {
      case (Empty, Empty) => None
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    }
  }

  def startWith[B >: A](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile(_._2 != None).forAll {
      case (Some(x), Some(y)) if x == y => true
      case _ => false
    }
  }

  def tails: Stream[Stream[A]] = {

    def tails(s: Stream[A]): Stream[Stream[A]] = {
      s match {
        case Empty => Empty
        case Cons(h, t) => Stream.cons[Stream[A]](t(), tails(t()))
      }
    }
    Stream.cons(this,tails(this))
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

  def onesUnfold: Stream[Int] = unfold(Unit)(_ => Some((1, Unit)))

  def fibsUnfold: Stream[Int] =
    Stream.cons(0,
      Stream.cons(1, unfold((0, 1))(s => Some((s._2 + s._1, (s._2, s._2 + s._1)))))
    )

  def constantUnfold[A](a: A): Stream[A] = unfold(Unit)(_ => Some((a, Unit)))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))


}
