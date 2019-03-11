package fpinscala.state

import org.scalatest.{Matchers, WordSpec}

class StateTest extends WordSpec with  Matchers {

    "State simulateMachine" should {
      "work as expected :)" in {
        val machine = Machine(true, 5, 10)
        val inputList = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
        val res = State.simulateMachine(inputList).run(machine)
        res._1 shouldBe (14, 1)
      }
    }
}
