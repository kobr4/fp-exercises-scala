package example

import org.scalatest._

class Exercise2Spec extends FlatSpec with Matchers {

  "Excercise2" should "compute fibo" in {
    Exercise2.fibtrec(1) should be(1)
    Exercise2.fibtrec(2) should be(1)
    Exercise2.fibtrec(3) should be(2)
    Exercise2.fibtrec(4) should be(3)
    Exercise2.fibtrec(5) should be(5)
    Exercise2.fibtrec(6) should be(8)
  }

  "Excercise2" should "find sorted and unsorted set" in {
    Exercise2.isSorted(Array(1, 2 , 3 ,4 ,5), (a: Int, b: Int) => a < b ) shouldBe true
    Exercise2.isSorted(Array(1, 2 , 3, 1 ,4 ,5), (a: Int, b: Int) => a < b ) shouldBe false
  }
}
