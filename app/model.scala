package turkotron

case class Game(
    id: String,
    url: String,
    position: Map[String, Boolean],
    turns: Int,
    finished: Boolean) {

  def step(pos: Map[String, Boolean]) = copy(
    turns = turns + 1,
    position = pos)
}

object Game {

  val initialPosition = (1 to 8).flatMap { rank =>
    ('a' to 'h') map { file =>
      s"$file$rank" -> (Set(1, 2, 7, 8) contains rank)
    }
  }.toMap
}

case class Setup(
    id: String,
    url: String) {

  def toGame = Game(
    id = id,
    url = url,
    position = Game.initialPosition,
    turns = 0,
    finished = false)
}
