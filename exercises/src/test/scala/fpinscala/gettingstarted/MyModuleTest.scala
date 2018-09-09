package fpinscala.gettingstarted

import org.scalatest._
import MyModule._

class MyModuleTest extends WordSpec with  Matchers {

  "Fibonacci" should {
    "give correct result for first 7 numbers" in {
      fib(0) shouldBe 0
      fib(1) shouldBe 1
      fib(2) shouldBe 1
      fib(3) shouldBe 2
      fib(4) shouldBe 3
      fib(5) shouldBe 5
      fib(6) shouldBe 8
    }

    "give correct result for 100" in {
      fib(45) shouldBe 1134903170
    }
  }
}
