package turkotron

import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

class PositionToMoveTest extends Specification {

  private val initial = Game.initialPosition

  "PositionToMove" should {

    "same position" in {
      PositionToMove(initial, initial) must_== Left("Positions are the same")
    }
    "no missing piece" in {
      val pos1 = initial
      val pos2 = initial + ("e4" -> true)
      PositionToMove(pos1, pos2) must_== Left("No missing piece")
    }
    "no added piece" in {
      val pos1 = initial
      val pos2 = initial - "e2"
      PositionToMove(pos1, pos2) must_== Left("No added piece")
    }
    "e2e4" in {
      val pos1 = initial
      val pos2 = initial - "e2" + ("e4" -> true)
      PositionToMove(pos1, pos2) must_== Right("e2e4")
    }
    "e2g7" in {
      val pos1 = initial
      val pos2 = initial - "e2" + ("g7" -> true)
      PositionToMove(pos1, pos2) must_== Right("e2g7")
    }
  }
}
