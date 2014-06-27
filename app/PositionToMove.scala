package turkotron

object PositionToMove {

  private type Square = String
  private type Color = Boolean
  private type Pos = Map[Square, Color]

  def apply(game: Game, position: Pos): String = {
    val pos1 = game.position
    val pos2 = position
    val found = for {
      missing <- findMissing(pos1, pos2)
      added <- findAdded(pos1, pos2)
    } yield s"$missing$added"
    found getOrElse sys.error(s"Can't find move\n$pos1\n$pos2")
  }

  private def findMissing(pos1: Pos, pos2: Pos): Option[Square] =
    (pos1.keySet diff pos2.keySet).headOption

  private def findAdded(pos1: Pos, pos2: Pos): Option[Square] =
    pos2 find {
      case (square, color) => (pos1 get square) != Some(color)
    } map (_._1)
}
