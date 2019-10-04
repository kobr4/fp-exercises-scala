package example

import example.Exercise8.Exercise8_9.Falsified
import example.Exercise8.{Exercise8_10, Exercise8_13}
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

  "Exercise8_6" should "generate an array of boolean of arbitrary length" in {

    val gen = Exercise8.Exercise8_4.boolean

    val value = gen.listOfN(Exercise8.Exercise8_4.choose(10, 20)).sample.run(SimpleRNG(1L))._1

    value.length should be >= 10
    value.length should be < 20
  }

  "Exercise8_7" should "generate from either first or secong generate" in {

    val gen1 = Exercise8.Exercise8_4.choose(1, 3)
    val gen2 = Exercise8.Exercise8_4.choose(7, 9)

    val value = Exercise8.Exercise8_4.union(gen1, gen2).listOfN(Exercise8.Exercise8_4.unit(10)).sample.run(SimpleRNG(1L))._1

    all(value) should ( (be >= 1 and be < 3) or (be >= 7 and be < 9) )
  }

  "Exercise8_8" should "generate from either first or second generator with weight" in {

    val gen1 = Exercise8.Exercise8_4.choose(1, 3)
    val gen2 = Exercise8.Exercise8_4.choose(7, 9)

    val value = Exercise8.Exercise8_4.weighted( (gen1, 0.2) , (gen2, 0.4) )
      .listOfN(Exercise8.Exercise8_4.unit(10)).sample.run(SimpleRNG(2L))._1

    all(value) should ( (be >= 1 and be < 3) or (be >= 7 and be < 9) )

  }

  "Exercise8_14" should "verify sorted list" in {

    val genOfInt = Exercise8_10.listOf1(Exercise8.Exercise8_4.choose(0, 50))

    val prop = Exercise8_13.forAll(genOfInt)(list => list.sorted.foldRight( (true, list.max) )((a, b) => (b._1 && a <= b._2, a))._1)

    val test = prop.run(10, 10, SimpleRNG(1L))

    test.isFalsified shouldBe false


  }

}
