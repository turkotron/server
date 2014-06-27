package turkotron

import akka.actor._
import play.api.libs.iteratee._
import play.api.libs.iteratee.Concurrent.Channel
import play.api.libs.json._
import play.api.libs.ws._
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

final class Server(val upstream: String) extends Actor {

  import Server._

  private var currentGame = Setup("", "").toGame

  private val (enumerator, channel) = Concurrent.broadcast[String]

  def receive = {

    case NewGame =>
      val mrSender = sender
      WS.url(s"$upstream/api/import/live").post(Map("a" -> Seq("b"))) foreach { res =>
        implicit val gameSetupReads = Json.reads[Setup]
        val game = res.json.as[Setup].toGame
        self ! SetGame(game)
        mrSender ! game
      }

    case SetGame(game) => currentGame = game

    case ConnectSnaper => sender ! enumerator

    case ClockSwitch   => channel push "!"

    case Image(file) =>
      val position = ImageToPosition(file)
      self ! AddPosition(position)

    case AddPosition(position) =>
      val move = PositionToMove(currentGame.position, position)
      WS.url(s"$upstream/api/import/live/${currentGame.id}/$move").post(Map("a" -> Seq("b")))
      currentGame = currentGame step position
  }
}

object Server {

  case object NewGame
  case class SetGame(game: Game)

  case object ConnectSnaper

  case object ClockSwitch

  case class Image(file: java.io.File)
  case class AddPosition(position: Map[String, Boolean])

  def make(upstream: String) =
    play.api.libs.concurrent.Akka.system.actorOf(Props(classOf[Server], upstream))
}
