package example

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorSystem, Props}

import language.postfixOps
import scala.concurrent.duration.TimeUnit

trait ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}

trait Callable[A] {
  def call: A
}

trait Future[A] {
  def get: A

  def get(timeout: Long, unit: TimeUnit): A

  def cancel(evenIfRunning: Boolean): Boolean

  def isDone: Boolean

  def isCancelled: Boolean
}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCanceled = false

    def cancel(evenIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = ???
  }

  private def map2future[A, B, C](af: Future[A], bf: Future[B])(f: (A, B) => C): Future[C] = {
    new Future[C] {
      override def get: C = f(af.get, bf.get)

      override def get(timeout: Long, unit: TimeUnit): C = {
        val started = System.currentTimeMillis
        val a = af.get(timeout, unit)
        val elapsed = System.currentTimeMillis - started
        val remaining = unit.toMillis(timeout) - elapsed
        val b = bf.get(remaining, unit)
        f(a, b)
      }

      override def cancel(evenIfRunning: Boolean): Boolean = af.isCancelled || bf.isCancelled

      override def isDone: Boolean = af.isDone && bf.isDone

      override def isCancelled: Boolean = af.isCancelled || bf.isCancelled
    }

  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })


  def asyncF[A, B](f: A => B): A => Par[B] = {
    (a: A) => lazyUnit(f(a))
  }

  def sequence[A](ps: List[Par[A]], acc: Par[List[A]] = unit[List[A]](List())): Par[List[A]] = {
    ps match {
      case Nil => acc
      case a :: tail => sequence(tail, map2(a, acc)((a: A, l: List[A]) => l :+ a))
    }
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val b = as.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(b))(_.flatten)
  }




}


