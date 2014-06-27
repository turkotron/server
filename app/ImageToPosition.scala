package turkotron

import java.io.File

object ImageToPosition {

  // takes an image file
  // returns a map of square -> piece color
  // true = white piece
  // false = black piece
  // e.g. Map("a1" -> true, "a2" -> true, "g4" -> false, "g7" -> false)
  def apply(file: File): Map[String, Boolean] = {
    // IMPLEMENT ME!
    val position = Game.initialPosition
    position
  }
}
