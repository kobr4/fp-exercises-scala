package example

import org.scalatest.{FlatSpec, Matchers}

class Exercise3Spec extends FlatSpec with Matchers {

  "Exercise3" should "provide tail list" in {

    Exercise3.tail(List(1, 2, 3, 4)) shouldBe List(2, 3, 4)

    a[RuntimeException] should be thrownBy Exercise3.tail(List())
  }

  "Exercise3" should "drop n elments of a list" in {

    Exercise3.drop(List(1, 2, 3, 4), 2) shouldBe List(3, 4)

    a[RuntimeException] should be thrownBy Exercise3.drop(List(1, 2, 3, 4), 6)
  }

  "Exercise3" should "dropWhile elements of a list matching predicate" in {

    Exercise3.dropWhile(List(1, 2, 3, 4, 5), (i: Int) => i < 2) shouldBe List(2, 3, 4, 5)

  }

  "Exercise3" should "drop last element of list" in {

    Exercise3.init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)

    a[RuntimeException] should be thrownBy Exercise3.init(Nil)
  }

  "Exercise3" should "return length of elements" in {

    Exercise3.length(List(1, 2, 3, 4, 5)) shouldBe 5
  }

  "Exercise3" should "foldLeft" in {

    Exercise3.foldLeft(List(1, 2, 3, 4, 5), "")((a, b) => s"$b$a") shouldBe
      List(1, 2, 3, 4, 5).foldLeft("")((a, b) => s"$a$b")


    Exercise3.foldRightL(List(1, 2, 3, 4, 5), "")((a, b) => s"$b$a") shouldBe Exercise3.foldRight(List(1, 2, 3, 4, 5), "")((a, b) => s"$b$a")
  }

  "Exercise3" should "reverse" in {

    Exercise3.reverse(List(1, 2, 3, 4, 5)) shouldBe List(5, 4, 3, 2, 1)

    Exercise3.reverseFold(List(1, 2, 3, 4, 5)) shouldBe List(5, 4, 3, 2, 1)
  }

  "Exercise3" should "concat" in {

    Exercise3.concat(List(List(1, 2), List(3, 4), List(5, 6))) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "Exercise3" should "add 1 to a list of Int" in {

    Exercise3.add(List(1, 2, 3, 4, 5)) shouldBe List(2, 3, 4, 5, 6)
  }

  "Exercise3" should "double to string" in {

    Exercise3.dToS(List(1.0, 2.0, 3.0, 4.0, 5.0)) shouldBe
      List(1.0.toString, 2.0.toString, 3.0.toString, 4.0.toString, 5.0.toString)
  }

  "Exercise3" should "map double to string" in {

    Exercise3.map(List(1.0, 2.0, 3.0, 4.0, 5.0))((d) => d.toString) shouldBe
      List(1.0.toString, 2.0.toString, 3.0.toString, 4.0.toString, 5.0.toString)
  }

  "Exercise3" should "filter out element" in {

    Exercise3.filter(List(1, 2, 3, 4, 5))((a) => a != 3) shouldBe List(1, 2, 4, 5)
  }

  "Exercise3" should "flatMap" in {

    Exercise3.flatMap(List(1, 2, 3))((i) => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)

    Exercise3.filterFlat(List(1, 2, 3, 4, 5))((a) => a != 3) shouldBe List(1, 2, 4, 5)
  }

  "Exercise3" should "mergeAdd" in {

    Exercise3.mergeAdd(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)

    Exercise3.zipWith(List(1, 2, 3), List(4, 5, 6))((a, b) => a + b) shouldBe List(5, 7, 9)
  }

  "Exercise3" should "find subseq" in {

    Exercise3.hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3)) shouldBe true

    Exercise3.hasSubsequence(List(1, 2, 3, 4, 5), List(2, 5)) shouldBe false

    Exercise3.hasSubsequence(List(1, 2, 3, 4, 5), List(7)) shouldBe false

    Exercise3.hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5, 6)) shouldBe false
  }

  "Exercise3" should "compute tree size" in {

    val tree = Branch[Int](Leaf(2), Branch(Leaf(3), Branch(Leaf(9), Leaf(2))))

    Exercise3.size(tree) shouldBe 7

    Exercise3.sizeT(tree) shouldBe 7

    Exercise3.maximum(tree) shouldBe 9
  }

  "Exercise3" should "compute tree depth" in {

    val tree = Branch[Int](Leaf(2), Branch(Leaf(3), Branch(Leaf(9), Leaf(2))))

    Exercise3.depth(tree) shouldBe 4
  }

  "Exercise3" should "process tree with map" in {

    val tree = Branch[Int](Leaf(2), Branch(Leaf(3), Branch(Leaf(9), Leaf(2))))

    Exercise3.size(Exercise3.map(tree)((a) => a.toString)) shouldBe 7
  }

  "Exercise3" should "maximum/depth/size fold" in {

    val tree = Branch[Int](Leaf(2), Branch(Leaf(3), Branch(Leaf(9), Leaf(2))))

    Exercise3.maximumFold(tree) shouldBe 9

    Exercise3.depthFold(tree) shouldBe 4

    Exercise3.sizeFold(tree) shouldBe 7
  }

  "Exercise3" should "process tree with mapFold" in {

    val tree = Branch[Int](Leaf(2), Branch(Leaf(3), Branch(Leaf(9), Leaf(2))))

    Exercise3.size(Exercise3.mapFold(tree)((a) => a.toString)) shouldBe 7
  }
}
