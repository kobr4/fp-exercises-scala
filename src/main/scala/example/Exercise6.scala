package example


trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = {
    State(s1 => {
      val (a, s2) = this.run(s1)
      (f(a), s2)
    })
  }

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(s1 => {
      val (a, s2) = this.run(s1)
      rb.map(b => f(a, b)).run(s2)
    })
  }

  def flatMap[B](g: A => State[S, B]): State[S, B] = {
    State(s1 => {
      val (a, s2) = this.map(a => g(a)).run(s1)
      a.run(s2)
    })
  }
}

object State {

  def unit[S, A](a: A) = State((s: S) => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    State(s => {
      fs match {
        case Nil => (List(), s)
        case a :: tail =>
          val (b, s2) = a.run(s)
          val (l, s3) = sequence(tail).run(s2)
          (l :+ b, s3)
      }
    })
  }

  def intsSeq(count: Int): State[RNG, List[Int]] = {
    sequence((for (i <- 1 to count) yield State((rng: RNG) => rng.nextInt)).toList)
  }

}

object Exercise6 {

  type Rand[+A] = RNG => (A, RNG)
  //type Rand[A] = State[RNG, A]

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val a = rng.nextInt
    if (a._1 == Int.MinValue) (Int.MaxValue, a._2) else if (a._1 < 0) (-a._1, a._2) else a
  }

  def double(rng: RNG): (Double, RNG) = {
    val a = nonNegativeInt(rng)
    (a._1.toDouble / Int.MaxValue.toDouble, a._2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val a = rng.nextInt
    val b = double(a._2)
    ((a._1, b._1), b._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i1, i2), rng2) = intDouble(rng)
    ((i2, i1), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val a = rng.nextInt
    val l = if (count > 1) ints(count - 1)(a._2) else (List(), a._2)
    (l._1 :+ a._1, l._2)
  }


  def doubleMap: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(_.nextInt, double)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs match {
        case Nil => (List(), rng)
        case f :: tail =>
          val (a, rng2) = f(rng)
          val (l, rng3) = sequence(tail)(rng2)
          (l :+ a, rng3)
      }
    }
  }

  def intsSeq(count: Int): Rand[List[Int]] = {
    sequence((for (i <- 1 to count) yield (rng: RNG) => rng.nextInt).toList)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i => {
      rng => {
        val mod = i % n
        if (i + (n - 1) - mod >= 0) (mod, rng) else nonNegativeLessThan(n)(rng)
      }
    })
  }

  def mapf[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => (rng) => (f(a), rng))
  }

  def map2f[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap[A, C](ra)(a => (rng) => {
      val (b, rng2) = rb(rng)
      (f(a, b), rng2)
    })
  }
}
