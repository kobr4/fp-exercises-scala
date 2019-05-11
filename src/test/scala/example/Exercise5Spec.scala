package example

import org.scalatest.{FlatSpec, Matchers}

class Exercise5Spec extends FlatSpec with Matchers {

  "Exercise5" should "transform stream into list" in {

    val streamOfInt: Stream[Int] = Stream(1, 2, 3)

    streamOfInt.toList shouldBe List(1, 2, 3)

  }

  "Exercise5" should "take n elements of a stream into list" in {

    val streamOfInt: Stream[Int] = Stream(1, 2, 3, 4, 5)

    streamOfInt.take(3).toList shouldBe List(1, 2, 3)

  }

  "Exercise5" should "drop n elements of a stream into list" in {

    val streamOfInt: Stream[Int] = Stream(1, 2, 3, 4, 5)

    streamOfInt.drop(3).toList shouldBe List(4, 5)
  }

  "Exercise5" should "take n elements of a stream matching predicate" in {

    val streamOfInt: Stream[Int] = Stream(1, 2, 3, 4, 5)

    streamOfInt.takeWhile(_ < 3).toList shouldBe List(1, 2)

    streamOfInt.takeWhileWithFold(_ < 3).toList shouldBe List(1, 2)
  }

  "Exercise5" should "test all elements in stream" in {

    val streamOfInt: Stream[Int] = Stream(1, 2, 3, 4, 5)

    streamOfInt.forAll(_ > 0) shouldBe true

    streamOfInt.forAll(_ < 5) shouldBe false
  }

  "Exercise5" should "headOption" in {

    val streamOfInt: Stream[Int] = Stream(1, 2, 3, 4, 5)

    streamOfInt.headOption should equal(streamOfInt.headOptionWithFold)

  }

  "Exercise5" should "map to string" in {

    val streamOfInt: Stream[Int] = Stream(1, 2, 3, 4, 5)

    streamOfInt.map(_.toString).toList shouldBe List("1", "2", "3", "4", "5")
  }

  "Exercise5" should "append elements" in {

    val streamOfInt: Stream[Int] = Stream(1, 2, 3, 4, 5)

    streamOfInt.append(List(6, 7, 8, 9)).toList shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  "Exercise5" should "filter elements" in {

    val streamOfInt: Stream[Int] = Stream(1, 2, 3, 4, 5)

    streamOfInt.filter(_ != 2).toList shouldBe List(1, 3, 4, 5)
  }

  "Exercise5" should "generate infinite list of elements" in {

    Exercise5.constant(1).take(5).toList shouldBe List(1, 1, 1, 1, 1)

    Exercise5.from(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)

    Exercise5.fibs.take(5).toList shouldBe List(0, 1, 1, 2, 3)

    Exercise5.fibsUnfold.take(5).toList shouldBe List(0, 1, 1, 2, 3)

    Exercise5.constantUnfold(1).take(5).toList shouldBe List(1, 1, 1, 1, 1)

    Exercise5.fromUnfold(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)

  }
}
