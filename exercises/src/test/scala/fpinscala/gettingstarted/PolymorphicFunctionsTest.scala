package fpinscala.gettingstarted

import fpinscala.gettingstarted.PolymorphicFunctions._
import org.scalatest.{Matchers, WordSpec}

class PolymorphicFunctionsTest extends WordSpec with  Matchers {

  "Is sorted" should {
    "be true for empty array" in {
      isSorted(Array(), (a: Int, b: Int) => a > b) shouldBe true
    }

    "be true for one element array" in {
      isSorted(Array(1), (a: Int, b: Int) => a > b) shouldBe true
    }

    "be true for sorted array" in {
      isSorted(Array(1, 2, 3, 4), (a: Int, b: Int) => a > b) shouldBe true
    }

    "be false for unsorted array" in {
      isSorted(Array(1, 2, 5, 4), (a: Int, b: Int) => a > b) shouldBe true
    }
  }
}

