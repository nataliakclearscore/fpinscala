package fpinscala.parallelism

import java.util.concurrent.Executors

import org.scalatest.{Matchers, WordSpec}

class ParTes extends WordSpec with  Matchers {

  "sequence" should {
    "apply order correctly" in {
      Par.run(Executors.newCachedThreadPool())(Par.sequence(List(Par.unit(1), Par.unit(2), Par.unit(3)))).get shouldBe List(1, 2, 3)
    }
  }

}
