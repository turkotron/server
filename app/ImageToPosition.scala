package turkotron

import play.api.Play

import java.io.File

import scala.concurrent._
import scala.sys.process._

import nl.flotsam.kmeans._

object ImageToPosition {

  case class ScriptsPath (dir: String) {
    def apply (scriptName: String) = {
      dir+"/"+scriptName;
    }
  }

  type Vec2 = Seq[Int]
  object Vec2 {
    def apply(x: Int, y: Int): Vec2 = Seq(x, y)
  }

  implicit object Vec2Space extends VectorSpace[Vec2] {
    def distance (a: Vec2, b: Vec2): Double = {
      val dx = a(0)-b(0)
      val dy = a(1)-b(1)
      math.sqrt(dx*dx+dy*dy)
    }
    def centroid (ps: Seq[Vec2]): Vec2 = {
      val sum = ps.fold(Vec2(0,0))( (a, b) => a.zip(b).map(zip => zip._1 + zip._2))
      sum.map(_ / ps.size)
    }
  }

  def parseVector (str: String): Vec2 = {
    str.split(",").map(_.toInt)
  }
  def vectorToString (vec: Vec2): String = {
    vec.foldLeft("")(_+","+_)
  }

  def colors (file: File)(implicit ec: ExecutionContext, scripts: ScriptsPath): Future[List[(Vec2, Int)]] = {
    Future {
      val cmd = (scripts("resize.sh") #< file) #| scripts("colors.sh")
      cmd.lineStream.map { str =>
        val split = str.split(" ")
        (parseVector(split(0)), split(1).toInt)
      }.toList
    }
  }

  def kmeans (list: List[(Vec2, Int)], k: Int = 4): List[(Int, (Vec2, Int))] = {
    val blueClusters = KMeans.cluster(list.filter(_._2==1).map(_._1), 3)
    val pinkClusters = KMeans.cluster(list.filter(_._2==2).map(_._1), 2)
    println(blueClusters)
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
  def apply(file: File)(implicit ec: ExecutionContext, scripts: ScriptsPath): Future[Map[String, Boolean]] = {
    val future = for {
      positions <- colors(file)
      clusters <- Future(kmeans(positions))
    } yield Map.empty[String, Boolean]

    future
  /*
    // IMPLEMENT ME!
    val position = Game.initialPosition
    position
    */
  }
}
