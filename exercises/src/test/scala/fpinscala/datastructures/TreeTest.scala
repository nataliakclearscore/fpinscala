package fpinscala.datastructures

import org.scalatest.{Matchers, WordSpec}
import fpinscala.datastructures.Tree._

class TreeTest extends WordSpec with  Matchers {

  "size" should {
    "return 1 for a leaf" in {
      val t = Leaf(1)
      Tree.size(t) shouldBe 1
      sizeFold(t) shouldBe 1
    }

    "return 3 for a tree with 2 leaves" in {
      val t = Branch(Leaf(1), Leaf(2))
      Tree.size(t) shouldBe 3
      sizeFold(t) shouldBe 3
    }

    "return size for a big with 2 leaves" in {
      val t = Branch(Leaf(1), Branch(Leaf(8), Branch(Leaf(5), Leaf(2))))
      Tree.size(t) shouldBe 7
      sizeFold(t) shouldBe 7
    }
  }

  "maximum" should {
    "return the correct value for a leaf" in {
      val t = Leaf(1)
      maximum(t)  shouldBe 1
    }

    "return the correct value for a tree with 2 leaves" in {
      val t = Branch(Leaf(1), Leaf(2))
      maximum(t) shouldBe 2
      maxFold(t) shouldBe 2
    }

    "return the correct value for a big tree" in {
      val t = Branch(Leaf(1), Branch(Leaf(8), Branch(Leaf(5), Leaf(2))))
      maximum(t) shouldBe 8
      maxFold(t) shouldBe 8
    }
  }

  "depth" should {
    "return the correct depth for a leaf" in {
      val t = Leaf(1)
      depth(t)  shouldBe 1
      depthFold(t)  shouldBe 1
    }

    "return the correct depth for a tree with 2 leaves" in {
      val t = Branch(Leaf(1), Leaf(2))
      depth(t) shouldBe 2
      depthFold(t) shouldBe 2
    }

    "return the correct depth for a big tree" in {
      val t = Branch(Leaf(1), Branch(Leaf(8), Branch(Leaf(5), Leaf(2))))
      depth(t) shouldBe 4
      depthFold(t) shouldBe 4
    }
  }

  "map" should {
    "work for a leaf" in {
      map(Leaf(1), (x: Int) => x.toString) shouldBe Leaf("1")
    }

    "work for a big tree" in {
      val t1 = Branch(Leaf(1), Branch(Leaf(8), Branch(Leaf(5), Leaf(2))))
      val t2 = Branch(Leaf(2), Branch(Leaf(9), Branch(Leaf(6), Leaf(3))))
      map(t1, (x: Int) => x + 1) shouldBe t2
    }
  }
}
