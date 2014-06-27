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

  def kmeans (list: List[(Vec2, Int)], k: Int = 4): Seq[Vec2] = {
    val blueCenters = KMeans.cluster(list.filter(_._2==1).map(_._1), 3).map(Vec2Space.centroid)
    val pinkCenter = Vec2Space.centroid(KMeans.cluster(list.filter(_._2==2).map(_._1), 2).sortBy(_.size).last)
    val centers = blueCenters :+ pinkCenter
    val center = Vec2Space.centroid(centers)

    def ccw(p1:Vec2, p2:Vec2, p3:Vec2) =
      (p2(0) - p1(0))*(p3(1) - p1(1)) - (p2(1) - p1(1))*(p3(0) - p1(0))

    val M = center
    val ordering = new Ordering[Vec2] {
      def compare (o1: Vec2, o2: Vec2) = {
        ccw(o1, o2, M)
      }
    }

    val ordered = centers.sorted(ordering)

    val (after, before) = ordered.splitAt(ordered.indexOf(pinkCenter))
    before ++ after
  }

  def perspective (file: File, corners: Seq[Vec2])(implicit ec: ExecutionContext): Future[_] = {
    Future {
      ()
    }
  }

  def perspective (file: File, points: Seq[Vec2]): Future[(File, Int)] = ???

  def pawnColor(file: File, dimension: Int, x: Int, y: Int): Option[Boolean] = ???

  def gridanalyze () {}

  // takes an image file
  // returns a map of square -> piece color
  // true = white piece
  // false = black piece
  // e.g. Map("a1" -> true, "a2" -> true, "g4" -> false, "g7" -> false)
  def apply(file: File)(implicit ec: ExecutionContext, scripts: ScriptsPath): Future[Map[String, Boolean]] = {
    val future = for {
      positions <- colors(file)
      corners <- Future(kmeans(positions))
      _ <- perspective(file, corners)
    } yield Map.empty[String, Boolean]

    future
  /*
    // IMPLEMENT ME!
    val position = Game.initialPosition
    position
    */
  }
}
