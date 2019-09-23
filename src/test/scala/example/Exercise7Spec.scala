package example

import akka.actor.ActorSystem
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.TimeUnit

class Exercise7Spec extends FlatSpec with Matchers {

  def es[A] = new ExecutorService {
    override def submit[A](a: Callable[A]): Future[A] = new Future[A] {

      override def get: A = a.call

      override def get(timeout: Long, unit: TimeUnit): A = ???

      override def cancel(evenIfRunning: Boolean): Boolean = ???

      override def isDone: Boolean = ???

      override def isCancelled: Boolean = ???
    }
  }

  "Exercise7" should "evaluate f async in" in {

    val value = Par.asyncF( (i: Int) =>  {
      i + 1
    } )

    value(1) shouldBe a[Par.Par[Int]]

    value(1)(es[Int]).get shouldBe 2
  }

  "Exercise7" should "sequence" in {

    val par = Par.sequence(List(Par.asyncF( (i: Int) => i + 1)(1), Par.asyncF( (i: Int) => i + 1)(2)))

    par shouldBe a[Par.Par[List[Int]]]

    par(es[Int]).get shouldBe List(2, 3)
  }

  "Exercise7" should "not block" in {

    implicit val as = ActorSystem()

    val p = Exercise7async.parMapAct(List.range(1, 1000))(math.sqrt(_))

    import java.util.concurrent.Executors

    Exercise7async.run(Executors.newFixedThreadPool(2))(p) shouldBe List.range(1, 1000).map(math.sqrt(_))

  }
}
