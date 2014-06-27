package turkotron

import play.api.Play

import java.io.File

import scala.concurrent._
import scala.sys.process._

object ImageToPosition {

  case class ScriptsPath (dir: String) {
    def apply (scriptName: String) = {
      dir+"/"+scriptName;
    }
  }

  case class Vec2 (x: Int, y: Int) {
    override def toString = s"$x,$y"
  }

  object Vec2 {
    def apply (str: String): Vec2 = {
      val split = str.split(",")
      println(split)
      Vec2(split(0).toInt, split(1).toInt)
    }
  }

  def colors (file: File)(implicit ec: ExecutionContext, scripts: ScriptsPath): Future[List[(Vec2, Int)]] = {
    Future {
      val cmd = scripts("colors.sh") #< file
      println(cmd)
      cmd.lineStream.map { str =>
        println(str)
        val split = str.split(" ")
        (Vec2(split(0)), split(1).toInt)
      }.toList
    }
  }

  def kmeans[Metadata] (list: List[(Vec2, Metadata)], k: Int = 4): List[(Int, (Vec2, Metadata))] = {
    println(list)
    List.empty
  }

  def perspective (topleft: Vec2, topright: Vec2, bottomright: Vec2, bottomleft: Vec2) {

  }
  
  def gridanalyze () {}

  // takes an image file
  // returns a map of square -> piece color
  // true = white piece
  // false = black piece
  // e.g. Map("a1" -> true, "a2" -> true, "g4" -> false, "g7" -> false)
  def apply(file: File)(implicit ec: ExecutionContext, scripts: ScriptsPath = ScriptsPath(Play.current.configuration.getString("scripts.dir").getOrElse("scripts"))): Map[String, Boolean] = {

    for {
      positions <- colors(file)
      clusters <- Future(kmeans(positions))
    } yield ()

    // IMPLEMENT ME!
    val position = Game.initialPosition
    position
  }
}
