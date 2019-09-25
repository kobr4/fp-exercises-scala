package example


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
  }

}
