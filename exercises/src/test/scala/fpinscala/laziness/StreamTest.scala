package fpinscala.laziness

import org.scalatest.{Matchers, WordSpec}

class StreamTest extends WordSpec with  Matchers {

  "toList" should {
    "convert stream to list" in {
      Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
    }

    "return empty list is stream is empty" in {
      Stream.empty.toList shouldBe List()
    }
  }

  "take" should {
    "return first n elemenst of the stream" in {
      Stream(1, 2, 3).take(2).toList shouldBe List(1, 2)
    }

    "return all element if n is greater than size" in {
      Stream(1, 2, 3).take(5).toList shouldBe List(1, 2, 3)
    }
  }

  "drop" should {
    "skip first n elemenst of the stream" in {
      Stream(1, 2, 3, 4).drop(2).toList shouldBe List(3, 4)
    }

    "return empty if n is greater than size" in {
      Stream(1, 2, 3).drop(5).toList shouldBe List()
    }
  }

  "takeWhile" should {
    "return all starting elements matching the predicate" in {
      Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList shouldBe List(1, 2, 3)
    }

    "return everything if the predicate is true for all elements" in {
      Stream(1, 2, 3).takeWhile(_ < 4).toList shouldBe List(1, 2, 3)
    }

    "return empty if the predicate is false for all elements" in {
      Stream(1, 2, 3).takeWhile(_ > 4).toList shouldBe List()
    }
  }

  "for all" should {
    "return true if the predicate is true for all elements" in {
      Stream(1, 2, 3).forAll(_ < 4) shouldBe true
    }

    "return false if the predicate is false for all elements" in {
      Stream(1, 2, 3).forAll(_ > 4) shouldBe false
    }

    "terminate early" in {
      val s = Stream.cons(1, Stream.cons(2, Stream.cons(5, Stream.cons(sys.error("error!"), Stream.empty))))
      s.forAll(_ < 4) shouldBe false
    }
  }

  "takeWhile via fold right" should {
    "return all starting elements matching the predicate" in {
      Stream(1, 2, 3, 4, 5).takeWhileViaFoldRight(_ < 4).toList shouldBe List(1, 2, 3)
    }

    "return everything if the predicate is true for all elements" in {
      Stream(1, 2, 3).takeWhileViaFoldRight(_ < 4).toList shouldBe List(1, 2, 3)
    }

    "return empty if the predicate is false for all elements" in {
      Stream(1, 2, 3).takeWhileViaFoldRight(_ > 4).toList shouldBe List()
    }
  }

  "headOption" should {
    "return None is empty" in {
      Stream.empty.headOption shouldBe None
    }

    "return first element is non empty" in {
      Stream.cons(1, Stream.cons(2, Stream.empty)).headOption shouldBe Some(1)
    }

    "terminate early" in {
      val s = Stream.cons(1, Stream.cons(2, Stream.cons(5, Stream.cons(sys.error("error!"), Stream.empty))))
      s.headOption shouldBe Some(1)
    }
  }

  "constant" should {
    "return an infinite stream of a given value" in {
      Stream.constant(5).take(3).toList shouldBe List(5, 5, 5)
      Stream.constant(1).take(5).toList shouldBe List(1, 1, 1, 1, 1)
    }
  }

  "from" should {
    "return an infinite stream of integers starting from" in {
      Stream.from(5).take(3).toList shouldBe List(5, 6, 7)
      Stream.from(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
    }
  }

  "fibs" should {
    "return an infinite stream of Fibonacci numbers" in {
      Stream.fibs().take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
    }
  }

  "ones via unfold" should {
    "return an infinite stream 1" in {
      Stream.onesViaUnfold.take(3).toList shouldBe List(1, 1, 1)
      Stream.onesViaUnfold.take(5).toList shouldBe List(1, 1, 1, 1, 1)
    }
  }

  "constant via unfold" should {
    "return an infinite stream of a given value" in {
      Stream.constantViaUnfold(5).take(3).toList shouldBe List(5, 5, 5)
      Stream.constantViaUnfold(1).take(5).toList shouldBe List(1, 1, 1, 1, 1)
    }
  }

  "from via unfold" should {
    "return an infinite stream of integers starting from" in {
      Stream.fromViaUnfold(5).take(3).toList shouldBe List(5, 6, 7)
      Stream.fromViaUnfold(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
    }
  }

  "fibs via unfold" should {
    "return an infinite stream of Fibonacci numbers" in {
      Stream.fibsViaUnfold().take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
    }
  }

  "map" should {
    "works in a given case" in {
      Stream(1, 2, 3).map(_.toString).toList shouldBe List("1", "2", "3")
    }
  }

  "map via unfold" should {
    "works in a given case" in {
      Stream(1, 2, 3).mapViaUnfold(_.toString).toList shouldBe List("1", "2", "3")
    }
  }

  "starts with" should {
    "return true if one stream starts with another" in {
      Stream(1, 2, 3).startsWith(Stream(1, 2)) shouldBe true
    }

    "return true if streams are the same" in {
      Stream(1, 2, 3).startsWith(Stream(1, 2, 3)) shouldBe true
    }

    "return false if one stream doesn't start with another" in {
      Stream(1, 2, 3).startsWith(Stream(2, 3)) shouldBe false
    }

    "works with infinite streams" in {
      Stream.ones.startsWith(Stream(1, 1, 1)) shouldBe true
    }
  }

  "take via unfold" should {
    "return first n element of the stream" in {
      Stream(1, 2, 3).takeViaUnfold(2).toList shouldBe List(1, 2)
    }

    "return all element if n is greater than size" in {
      Stream(1, 2, 3).takeViaUnfold(5).toList shouldBe List(1, 2, 3)
    }
  }

  "takeWhile via unfold" should {
    "return all starting elements matching the predicate" in {
      Stream(1, 2, 3, 4, 5).takeWhileViaUnfold(_ < 4).toList shouldBe List(1, 2, 3)
    }

    "return everything if the predicate is true for all elements" in {
      Stream(1, 2, 3).takeWhileViaUnfold(_ < 4).toList shouldBe List(1, 2, 3)
    }

    "return empty if the predicate is false for all elements" in {
      Stream(1, 2, 3).takeWhileViaUnfold(_ > 4).toList shouldBe List()
    }
  }

  "zipAll" should {
    "works for streams of equal size" in {
      Stream(1, 2, 3).zipAll(Stream(4, 5, 6)).toList shouldBe List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6)))
    }

    "works if the first stream is shorter" in {
      Stream(1).zipAll(Stream(4, 5, 6)).toList shouldBe List((Some(1), Some(4)), (None, Some(5)), (None, Some(6)))
    }

    "works if the second stream is shorter" in {
      Stream(1, 2, 3).zipAll(Stream(4)).toList shouldBe List((Some(1), Some(4)), (Some(2), None), (Some(3), None))
    }

    "works with infinite streams" in {
      Stream.constant(1).zipAll(Stream.constant(2)).take(2).toList shouldBe List((Some(1), Some(2)), (Some(1), Some(2)))
    }
  }

  "tails" should {
    "works in a given case" in {
      Stream(1, 2, 3).tails().map(_.toList).toList shouldBe List(List(1, 2, 3), List(2, 3), List(3))
    }
  }
}
