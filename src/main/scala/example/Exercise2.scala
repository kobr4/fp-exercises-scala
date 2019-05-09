package example

import scala.annotation.tailrec

object Exercise2 {

  def fibtrec(t: Int): Int = {
    @tailrec
    def fib(t: Int, n: Int, acc: Int, acc2: Int): Int = {
      n match {
        case _ if t == 0 => 0
        case _ if t == 1 => 1
        case _ if n == (t - 1) => acc + acc2
        case _ => fib(t, n + 1, acc + acc2, acc)
      }
    }

    fib(t, 1, 1, 0)
  }

  def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.foldLeft((as.head, true))((a, b) => {
      if (!a._2) {
        a
      } else {
        (b, ordered(a._1, b))
      }
    })
  }._2


  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n == as.length)
        true
      else
        if (!ordered(as(n-1), as(n)))
          false
      else loop(n+1)
    }
    loop(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}
