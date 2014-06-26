package controllers

import akka.pattern.ask
import play.api._
import play.api.libs.EventSource
import play.api.libs.iteratee._
import play.api.mvc._
import play.api.Play.current
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import turkotron.{ Server, Game }

object Application extends Controller {

  private implicit val timeout = akka.util.Timeout(1.second)

  private val server = Server.make(
    Play.configuration.getString("upstream.url") getOrElse "http://en.lichess.org"
  )

  def create = Action.async {
    server ? Server.NewGame mapTo manifest[Game] map { game =>
      println("create " + game.url)
      Ok(game.url)
    }
  }

  def connectSnapper = Action.async {
    server ? Server.ConnectSnaper mapTo manifest[Enumerator[String]] map { enum =>
      Ok.chunked(enum &> EventSource()) //.as("text/event-stream")
    }
  }

  def clockSwitch = Action {
    server ! Server.ClockSwitch
    println("clock!")
    Ok("ok")
  }
}
