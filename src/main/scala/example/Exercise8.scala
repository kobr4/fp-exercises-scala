package example

import example.Exercise8.Exercise8_9.Prop.{FailedCase, SuccessCount}


object Exercise8 {

  object Exercise8_3 {

    trait Prop {

      def check: Boolean

      def &&(p: Prop): Prop = {
        val f = () => check
        new Prop {
          override def check: Boolean = f() && p.check
        }
      }
    }

  }

  object Exercise8_4 {

    case class SGen[A](forSize: Int => Gen[A])

    case class Gen[A](sample: State[RNG, A]) {

      def flatMap[B](f: A => Gen[B]): Gen[B] = {
        Gen(State({ rng =>
          val s = sample.run(rng)
          val s2 = f(s._1).sample.run(s._2)
          (s2._1, s2._2)
        }))
      }

      def listOfN(size: Gen[Int]): Gen[List[A]] = {
        size.flatMap(n => Exercise8_4.listOfN(n, this))
      }

      def unsigned: SGen[A] = {
        SGen((_) => this)
      }
    }

    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(State({ rng: RNG => rng.nextInt }).map(s => (Math.abs(s) % (stopExclusive - start)) + start))
    }

    def unit[A](a: => A): Gen[A] = {
      Gen(State({ rng: RNG => (a, rng) }))
    }

    def boolean: Gen[Boolean] =
      Gen(State({ rng: RNG => rng.nextInt }).map(s => if (s % 2 == 0) true else false))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {

      def listOfNested(n: Int, g: Gen[A], rng: RNG, l: List[A]): List[A] = {
        if (n == 0)
          l
        else {
          val state = g.sample.run(rng)
          listOfNested(n - 1, g, state._2, state._1 :: l)
        }
      }

      Gen(State({ rng => (listOfNested(n, g, rng, List()), rng) }))
    }

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
      Gen(State({ rng: RNG => rng.nextInt }).flatMap(i => if (i % 2 == 0) g1.sample else g2.sample))
    }

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      Gen(State({ rng: RNG => Exercise6.double(rng) })
        .flatMap(d => if (d * (g1._2 + g2._2) < g1._2) g1._1.sample else g2._1.sample))
    }

  }

  object Exercise8_9 {

    type TestCases = Int


    object Prop {
      type FailedCase = String
      type SuccessCount = Int
    }

    sealed trait Result {

      def isFalsified: Boolean
    }

    case object Passed extends Result {

      def isFalsified = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {

      def isFalsified = true
    }

    case class Prop(run: (TestCases, RNG) => Result) {

      def &&(p: Prop): Prop = {
        val p0 = this
        Prop((cases, rng) => {
          (p0.run(cases, rng), p.run(cases, rng)) match {
            case res@(Falsified(f, s), _) => res._1
            case res@(_, Falsified(f, s)) => res._2
            case _ => Passed
          }
        })
      }

      def ||(p: Prop): Prop = {
        val p0 = this
        Prop((cases, rng) =>
          (p0.run(cases, rng), p.run(cases, rng)) match {
            case res@(Falsified(f, s), Falsified(f2, s2)) => res._1
            case _ => Passed
          }
        )
      }
    }

  }

  object Exercise8_10 {

    import Exercise8_4._

    def listOf[A](g: Gen[A]): SGen[List[A]] = {

      SGen(size => listOfN(size, g))
    }

    def listOf1[A](g: Gen[A]): SGen[List[A]] = {

      SGen(size => listOfN(size+1, g))
    }
  }

  object Exercise8_13 {

    import Exercise8_9.TestCases
    import Exercise8_9.Result
    import Exercise8_4._


    type MaxSize = Int

    case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

      def &&(p: Prop): Prop = {
        val p0 = this
        Prop((max, cases, rng) => {
          (p0.run(max, cases, rng), p.run(max, cases, rng)) match {
            case res@(Falsified(f, s), _) => res._1
            case res@(_, Falsified(f, s)) => res._2
            case _ => Passed
          }
        })
      }

      def ||(p: Prop): Prop = {
        val p0 = this
        Prop((max, cases, rng) =>
          (p0.run(max, cases, rng), p.run(max, cases, rng)) match {
            case res@(Falsified(f, s), Falsified(f2, s2)) => res._1
            case _ => Passed
          }
        )
      }
    }

    object Prop {
      type FailedCase = String
      type SuccessCount = Int
    }

    sealed trait Result {

      def isFalsified: Boolean
    }

    case object Passed extends Result {

      def isFalsified = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {

      def isFalsified = true
    }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll[A](g.forSize)(f)


    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Exercise5.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop(
      (max, n, rng) => randomStream(as)(rng).zipAll(Exercise5.from(0)).take(n).map {
        case (Some(a), Some(i)) => Passed
          try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.filter(_.isFalsified).headOption.getOrElse(Passed)
    )



    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max, n, rng) =>
        val casesPerSize = (n + (max - 1)) / max
        val props: Stream[Prop] = Exercise5.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop = props.map(p => Prop((max, _, rng) => p.run(max, casesPerSize, rng))).toList.reduce(_ && _)
        prop.run(max, n, rng)
    }

    def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit = {
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
      }
    }
  }

}
