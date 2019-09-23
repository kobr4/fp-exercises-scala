package example

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorSystem, Props}

import scala.annotation.tailrec
import scala.concurrent.duration.TimeUnit


object Exercise7async {

  //import java.util.concurrent._
  /*
    sealed trait Callable[A] {
      def call: A
    }
  */
  sealed trait Future[A] {
    def apply(k: A => Unit): Unit
  }

  /*
    sealed trait ExecutorService {
      def submit[A](a: Callable[A]): Future[A]
    }
  */
  type Par[A] = java.util.concurrent.ExecutorService => Future[A]

  /*
    def es[A] = new ExecutorService {
      override def submit[A](a: Callable[A]) : Future[A] = new Future[A] {
        override def apply(k: A => Unit): Unit = ???
      }
    }
  */
  /*
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCanceled = false

    def cancel(evenIfRunning: Boolean): Boolean = false

  }
*/
  def unit[A](a: A): Par[A] = {
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit = cb(a)
      }
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => a(es)


  def asyncF[A, B](f: A => B): A => Par[B] = {
    (a: A) => lazyUnit(f(a))
  }

  def parMapAct[A, B](ps: List[A])(f: A => B)(implicit actorSystem: ActorSystem): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequenceAct(fbs)
  }

  @tailrec
  def sequenceAct[A](ps: List[Par[A]], acc: Par[List[A]] = unit[List[A]](List()))(implicit actorSystem: ActorSystem): Par[List[A]] = {
    ps match {
      case Nil => acc
      case a :: tail => sequenceAct(tail, map2act(a, acc)((a: A, l: List[A]) => l :+ a))
    }
  }

  def run[A](es: java.util.concurrent.ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown() }
    latch.await()
    ref.get
  }

  def eval(es: java.util.concurrent.ExecutorService)(r: => Unit): Unit = es.submit(new java.util.concurrent.Callable[Unit] {
    def call = r
  })

  def map2act[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C)(implicit actorSystem: ActorSystem): Par[C] = {
    es =>
      new Future[C] {

        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None

          val act = actorSystem.actorOf(Props(new Actor {
            override def receive: Receive = {
              case Left(a: A) => br match {
                case None => ar = Some(a)
                case Some(b) => eval(es)(cb(f(a, b)))
              }

              case Right(b: B) => ar match {
                case None => br = Some(b)
                case Some(a) => eval(es)(cb(f(a, b)))
              }
            }
          }))
          p(es)(a => act ! Left(a))
          p2(es)(b => act ! Right(b))
        }

      }
  }

}
