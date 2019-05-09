package example

import scala.annotation.tailrec


sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Exercise3 {

  def tail[A](a: List[A]): List[A] = {
    a match {
      case Nil => sys.error("Boo!")
      case _ :: next => next
    }
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    n match {
      case _ if l == Nil => sys.error("Boo!")
      case 0 => l
      case _ => drop(tail(l), n - 1)
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    if (l == Nil) sys.error("Boo !")
    if (!f(l.head)) {
      l
    } else {
      dropWhile(tail(l), f)
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("Boo !")
      case _ :: tail if tail == Nil => Nil
      case e :: tail => e :: init(tail)
    }
  }

  def length[A](l: List[A]): Int = {
    l.foldRight(0)((_, b) => b + 1)
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case a :: tail => foldLeft(tail, f(a, z))(f)
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case a :: tail => f(a, foldRight(tail, z)(f))
    }
  }

  def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    val g: (A) => B => B = (a: A) => (b: B) => f(a, b)
    foldLeft(as, (b: B) => b)(
      (a, b) => g(a).andThen(b)
    )(z)
  }

  def reverse[A](l: List[A]): List[A] = {
    @tailrec
    def reverse(l: List[A], acc: List[A]): List[A] = {
      l match {
        case Nil => acc
        case a :: tail => reverse(tail, a :: acc)
      }
    }

    reverse(l, Nil)
  }

  def reverseFold[A](l: List[A]): List[A] = {
    l.foldLeft(Nil: List[A])((lout, a) => a :: lout)
  }

  def concat[A](l: List[List[A]]): List[A] = {
    l.foldLeft(Nil: List[A])((lout, le) => lout ++ le)
  }

  def add(l: List[Int]): List[Int] = {
    l.foldLeft(Nil: List[Int])((lout, i) => lout :+ (i + 1))
  }

  def dToS(l: List[Double]): List[String] = {

    @tailrec
    def dToS(l: List[Double], acc: List[String]): List[String] = {
      l match {
        case Nil => acc
        case d :: tail => dToS(tail, acc :+ d.toString)
      }
    }

    dToS(l, Nil)
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    as.foldLeft(Nil: List[B])((acc, a) => acc :+ f(a))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as.foldLeft(Nil: List[A])((acc, a) => if (f(a)) acc :+ a else acc)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    as.foldLeft(Nil: List[B])((acc, a) => acc ++ f(a))
  }

  def filterFlat[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((a) => if (f(a)) List(a) else List.empty)
  }

  def mergeAdd(l1: List[Int], l2: List[Int]): List[Int] = {

    @tailrec
    def mergeAdd(l1: List[Int], l2: List[Int], lout: List[Int]): List[Int] = {
      (l1, l2) match {
        case (Nil, Nil) => lout
        case (i1 :: tail1, i2 :: tail2) => mergeAdd(tail1, tail2, lout :+ (i1 + i2))
        case _ => sys.error("Invalid merge")
      }
    }

    mergeAdd(l1, l2, Nil)
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {

    @tailrec
    def zipWith(l1: List[A], l2: List[A], lout: List[A]): List[A] = {
      (l1, l2) match {
        case (Nil, Nil) => lout
        case (i1 :: tail1, i2 :: tail2) => zipWith(tail1, tail2, lout :+ f(i1, i2))
        case _ => sys.error("Invalid merge")
      }
    }

    zipWith(l1, l2, Nil)
  }

  def hasSubsequence[A](l1n: List[A], l2n: List[A]): Boolean = {

    @tailrec
    def hasSubseq(l1: List[A], l2: List[A]): Boolean = {
      (l1, l2) match {
        case (Nil, _) => false
        case (_, Nil) => true
        case (i1 :: tail1, i2 :: tail2) if i1 == i2 => hasSubseq(tail1, tail2)
        case (_ :: tail1, _ :: _) => hasSubseq(tail1, l2n)
      }
    }

    hasSubseq(l1n, l2n)
  }

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def sizeT[A](t: Tree[A]): Int = {

    @tailrec
    def sizeRT(t: Tree[A], l: List[Tree[A]], acc: Int): Int = {
      (t, l) match {
        case (Leaf(_), a :: tail) => sizeRT(a, tail, acc + 1)
        case (Leaf(_), Nil) => acc + 1
        case (Branch(_, right), _) => sizeT(right, l, acc)
      }
    }

    @tailrec
    def sizeT(t: Tree[A], l: List[Tree[A]], acc: Int): Int = {
      (t, l) match {
        case (Leaf(_), a :: tail) => sizeRT(a, tail, acc + 1)
        case (Leaf(_), Nil) => acc + 1
        case (Branch(left, _), _) => sizeT(left, l :+ t, acc + 1)
      }
    }

    sizeT(t, Nil, 0)
  }


  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(a) => a
      case Branch(left, right) => maximum(left).max(maximum(right))
    }
  }

  def depth(t: Tree[Int], level: Int = 0): Int = {
    t match {
      case Leaf(a) => level + 1
      case Branch(left, right) => depth(left, level + 1).max(depth(right, level + 1))
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def fold[A, B](t: Tree[A])(f: A => B, g: (B, B) => B): B = {
    t match {
      case Leaf(a) => f(a)
      case Branch(left, right) => g(fold(left)(f, g), fold(right)(f, g))
    }
  }

  def maximumFold(t: Tree[Int]): Int = {

    val f = (a: Int) => a

    val g = (a: Int, b: Int) => a.max(b)

    fold(t)(f, g)
  }

  def depthFold(t: Tree[Int]): Int = {

    val f = (a: Int) => 1

    val g = (a: Int, b: Int) => a.max(b) + 1

    fold(t)(f, g)
  }

  def sizeFold(t: Tree[Int]): Int = {

    val f = (a: Int) => 1

    val g = (a: Int, b: Int) => a + b + 1

    fold(t)(f, g)
  }

  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {

    val h = (a: A) => Leaf(f(a))

    val g = (a: Tree[B], b: Tree[B]) => Branch(a, b)

    fold(t)(h, g)
  }
}
