package example

import scala.annotation.tailrec

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }


  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case _ => ob
  }

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Right(a) => b.map(vb => f(a, vb))
    case Left(e) => Left(e)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]


object Exercise4 {

  def variance(xs: Seq[Double]): Option[Double] = {
    (if (xs.nonEmpty)
      Some(xs.sum / xs.length)
    else
      None).map(m => xs.map(x => math.pow(x - m, 2)).sum / xs.length)
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(va => b.map(vb => f(va, vb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

    @tailrec
    def sequence(a: List[Option[A]], acc: List[A]): Option[List[A]] = {
      a match {
        case Nil => Some(acc)
        case e :: tail => e match {
          case None => None
          case Some(v) => sequence(tail, acc :+ v)
        }
      }
    }

    sequence(a, Nil)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

    @tailrec
    def traverse(a: List[A], acc: List[B]): Option[List[B]] = {
      a match {
        case Nil => Some(acc)
        case e :: tail => f(e) match {
          case None => None
          case Some(v) => traverse(tail, acc :+ v)
        }
      }
    }

    traverse(a, Nil)
  }

  def sequenceTrav[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(a => a)

  def sequenceE[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {

    @tailrec
    def sequence(es: List[Either[E, A]], acc: List[A]): Either[E, List[A]] = {
      es match {
        case Nil => Right(acc)
        case e :: tail => e match {
          case Left(err) => Left(err)
          case Right(a) => sequence(tail, acc :+ a)
        }
      }
    }

    sequence(es, Nil)
  }

  def traverseE[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {

    @tailrec
    def traverse(as: List[A], acc: List[B]): Either[E, List[B]] = {
      as match {
        case Nil => Right(acc)
        case e :: tail => f(e) match {
          case Left(err) => Left(err)
          case Right(v) => traverse(tail, acc :+ v)
        }
      }
    }

    traverse(as, Nil)
  }
}
