package turkotron

case class Game(
  id: String,
  url: String,
  turns: Int,
  finished: Boolean)

case class Setup(
    id: String,
    url: String) {

  def toGame = Game(id, url, 0, false)
}
