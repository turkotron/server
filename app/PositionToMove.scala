package turkotron

object PositionToMove {

  private type Error = String // oh well.
  private type Square = String
  private type Color = Boolean
  private type Pos = Map[Square, Color]

  def apply(pos1: Pos, pos2: Pos): Either[Error, Square] =
    if (pos1 == pos2) Left("Positions are the same")
    else for {
      missing <- findMissing(pos1, pos2).toRight("No missing piece").right
      added <- findAdded(pos1, pos2).toRight("No added piece").right
    } yield s"$missing$added"

  private def findMissing(pos1: Pos, pos2: Pos): Option[Square] =
    (pos1.keySet diff pos2.keySet).headOption

  private def findAdded(pos1: Pos, pos2: Pos): Option[Square] =
    pos2 find {
      case (square, color) => (pos1 get square) != Some(color)
    } map (_._1)
}
