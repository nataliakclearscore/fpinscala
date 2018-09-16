package fpinscala.datastructures

import org.scalatest.{Matchers, WordSpec}
import fpinscala.datastructures.List._

class ListTest extends WordSpec with  Matchers {

  "tail" should {
    "return Nil for one element list" in {
      tail(List(1)) shouldBe Nil
    }

    "return correct tail for 2 element list" in {
      tail(List(1, 2)) shouldBe List(2)
    }

    "return correct tail for 3 element list" in {
      tail(List("s", "ajc", "tt")) shouldBe List("ajc", "tt")
    }

    "throw exception for the empty list" in {
      assertThrows[IllegalArgumentException] {
        tail(Nil)
      }
    }
  }

  "set head" should {
    "replace head on one element list" in {
      setHead(List(1), 2) shouldBe List(2)
    }

    "replace head on 2 element list" in {
      setHead(List(1, 2), 3) shouldBe List(3, 2)
    }

    "return correct tail for 3 element list" in {
      setHead(List("s", "ajc", "tt"), "xx") shouldBe List("xx", "ajc", "tt")
    }

    "throw exception for the empty list" in {
      assertThrows[IllegalArgumentException] {
        setHead(Nil, "m")
      }
    }
  }

  "drop" should {
    "return correct list then n is less the size" in {
      drop(List(1, 2, 3, 4, 5), 3) shouldBe List(4, 5)
    }

    "return empty list then n equals to the size" in {
      drop(List(1, 2, 3, 4, 5), 5) shouldBe Nil
    }

    "drop 0 works on the empty list" in {
      drop(Nil, 0) shouldBe Nil
    }

    "throw exception for the empty list" in {
      assertThrows[IllegalArgumentException] {
        drop(Nil, 1)
      }
    }
  }

  "drop while" should {
    "return correct list then n is less the size" in {
      dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 4) shouldBe List(4, 5)
    }

    "return empty then the condition is true" in {
      dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 40) shouldBe Nil
    }

    "drop works on the empty list" in {
      dropWhile(Nil, (x: Int) => x < 3) shouldBe Nil
    }
  }

  "init" should {
    "return Nil for one element list" in {
      init(List(1)) shouldBe Nil
    }

    "return correct tail for 2 element list" in {
      init(List(1, 2)) shouldBe List(1)
    }

    "return correct tail for 3 element list" in {
      init(List("s", "ajc", "tt")) shouldBe List("s", "ajc")
    }

    "throw exception for the empty list" in {
      assertThrows[IllegalArgumentException] {
        init(Nil)
      }
    }
  }

  "length" should {
    "return 1 for one element list" in {
      List.length(List(1)) shouldBe 1
    }

    "return 2  2 element list" in {
      List.length(List(1, 2)) shouldBe 2
    }

    "return 3 tail for 3 element list" in {
      List.length(List("s", "ajc", "tt")) shouldBe 3
    }

    "throw 0 for an empty list" in {
      List.length(Nil) shouldBe 0
    }
  }

  "fold left" should {
    "be able to sum elements" in {
      foldLeft(List(1, 2, 4), 0)(_ + _) shouldBe 7
    }

    "be able to sum elements of empty list" in {
      foldLeft(Nil:List[Int], 0)(_ + _) shouldBe 0
    }

    "be able to multiply elements" in {
      foldLeft(List(1, 2, 4), 1)(_ * _) shouldBe 8
    }

    "be able to multiply elements of empty list" in {
      foldLeft(Nil:List[Int], 1)(_ * _) shouldBe 1
    }

    "reverse list" in {
      foldLeft(List(1, 2, 3), Nil:List[Int])((l, i) => Cons(i, l)) shouldBe List(3, 2, 1)
    }
  }

  "map" should {
    "be able to transform list on ints by adding 1 to each element" in {
      map(List(1, 2, 4))(_ + 1) shouldBe List(2, 3, 5)
    }

    "be able to convert list on doubles to list of strings" in {
      map(List(1.0, 2.0, 4.0))(_.toString) shouldBe List("1.0", "2.0", "4.0")
    }
  }
}