package fpinscala.errorhandling

import org.scalatest.{Matchers, WordSpec}
import fpinscala.errorhandling.Option._

class OptionTest extends WordSpec with  Matchers {
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case _: Exception => None }
  }

  "parseInt implemented with sequence" should {
    def parseInt(a: List[String]): Option[List[Int]] = {
      sequence(a map (s=> Try(s.toInt)))
    }

    "work with traverse when all strings can be converted to ints" in {
      parseInt(List("1", "3", "2")) shouldBe Some(List(1, 3, 2))
    }

    "return None when there is non int in the list" in {
      parseInt(List("1", "a", "2")) shouldBe None
    }
  }

  "parseInt implemented with traverse" should {
    def parseInt(a: List[String]): Option[List[Int]] = {
      traverse(a)(s=> Try(s.toInt))
    }

    "work with traverse when all strings can be converted to ints" in {
      parseInt(List("1", "3", "2")) shouldBe Some(List(1, 3, 2))
    }

    "return None when there is non int in the list" in {
      parseInt(List("1", "a", "2")) shouldBe None
    }
  }
}
