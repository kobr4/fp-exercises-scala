package example

import org.scalatest.{FlatSpec, Matchers}

class Exercise8Spec extends FlatSpec with Matchers {

  "Exercise8_3" should "prop &&" in {

    import Exercise8.Exercise8_3._

    val p1 = new Prop {
      override def check: Boolean = true
    }

    val p2 = new Prop {
      override def check: Boolean = false
    }


    ( p1 && p2 ).check shouldBe false
  }

  "Exercise8_4" should "generate int from range" in {

    val gen = Exercise8.Exercise8_4.choose(10, 20)

    val value = gen.sample.run(SimpleRNG(1L))._1
    value should be > 10
    value should be < 20
  }

  "Exercise8_4" should "generate an array of int between 10 and 20" in {

    val gen = Exercise8.Exercise8_4.listOfN(8, Exercise8.Exercise8_4.choose(10, 20))

    val value = gen.sample.run(SimpleRNG(1L))._1

    all(value) should be >= 10
    all(value) should be < 20

  }

  "Exercise8_8" should "generate an array of boolean of arbitrary length" in {

    val gen = Exercise8.Exercise8_4.boolean

    val value = gen.listOfN(Exercise8.Exercise8_4.choose(10, 20)).sample.run(SimpleRNG(1L))._1

    value.length should be >= 10
    value.length should be < 20
  }

}
