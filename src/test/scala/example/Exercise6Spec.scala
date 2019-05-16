package example

import org.scalatest.{FlatSpec, Matchers}

class Exercise6Spec extends FlatSpec with Matchers {

  "Exercise6" should "generate random values" in {

    Exercise6.nonNegativeInt(SimpleRNG(1L))._1 shouldBe >(0)

    Exercise6.double(SimpleRNG(1L))._1 shouldBe <(1.0)

    Exercise6.ints(20)(SimpleRNG(1000L))._1.length shouldBe 20

  }

  "Exercise6" should "generate random values with map" in {

    Exercise6.doubleMap(SimpleRNG(1L))._1 shouldBe <(1.0)

    Exercise6.randIntDouble(SimpleRNG(1L))._1 shouldBe an[(Int, Double)]

    Exercise6.ints(20)(SimpleRNG(1000L))._1 shouldBe Exercise6.intsSeq(20)(SimpleRNG(1000L))._1

    Exercise6.nonNegativeLessThan(100)(SimpleRNG(1L))._1 shouldBe <(100)
  }

  "Exercise6" should "generalized ops" in {

    State.intsSeq(20).run(SimpleRNG(1000L))._1 shouldBe Exercise6.intsSeq(20)(SimpleRNG(1000L))._1
  }

}

