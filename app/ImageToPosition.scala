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
      val cmd = scripts("colors.sh") #< file
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

  def randomFile (): File = {
    new java.io.File("/tmp/turktron."+math.random+".png") // FIXME
  }

  def perspective (file: File, points: Seq[Vec2])(implicit ec: ExecutionContext): Future[(File, Int)] = {
    Future {
      (file, 200)
    }
  }

  def pawnColor(file: File, dimension: Int, x: Int, y: Int)(implicit ec: ExecutionContext, scripts: ScriptsPath): Future[Option[Boolean]] = {
    Future {
      None
    }
  }

  val Xs = "abcdefgh".toList.map(_.toString)
  val Ys = "87654321".toList.map(_.toString)
  def allPawns(gridFile: File, dimension: Int)(implicit ec: ExecutionContext, scripts: ScriptsPath)
    : Future[List[(String, Option[Boolean])]] = {
    Future.sequence(
      (
        for {
          x <- 0 until 8
          y <- 0 until 8
        } yield {
          val pos: String = Xs(x)+Ys(y)
          pawnColor(gridFile, dimension, x * dimension / 8, y * dimension / 8).map { result =>
            (pos, result)
          }
        }
      ).toList
    )
  }

  // takes an image file
  // returns a map of square -> piece color
  // true = white piece
  // false = black piece
  // e.g. Map("a1" -> true, "a2" -> true, "g4" -> false, "g7" -> false)
  def apply(file: File)(implicit ec: ExecutionContext, scripts: ScriptsPath): Future[Map[String, Boolean]] = {
    val resized = randomFile()
    (scripts("colors.sh") #< file) #> resized !
    
    for {
      positions <- colors(resized)
      corners <- Future(kmeans(positions))
      (gridFile, size) <- perspective(resized, corners)
      results <- allPawns(gridFile, size)
    }
    yield results.flatMap { case (pos, valueOpt) =>
      valueOpt.map { value => (pos, value) }
    }.toMap
  }
}
