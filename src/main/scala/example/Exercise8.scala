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

}
