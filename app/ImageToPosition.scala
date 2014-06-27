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

  def resize (file: File, size: Int = 200)(implicit ec: ExecutionContext, scripts: ScriptsPath): Future[File] = {
    Future {
      val resized = randomFile()
      ((Seq(scripts("resize.sh"), size.toString) #< file) #> resized).run().exitValue()
      resized
    }
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

  def perspective (file: File, points: Seq[Vec2])(implicit ec: ExecutionContext, scripts: ScriptsPath): (File, Int) = {
    points match {
      case Seq(tl, bl, br, tr) => {

        val width = math.min(
          math.min(
            Math.abs(tl(0) - tr(0)),
            Math.abs(bl(0) - br(0))
          ),
          math.min(
            Math.abs(tl(1) - bl(1)),
            Math.abs(tr(1) - br(1))
          )
        )

        println("Perspective width : " + width.toString)

        val output = randomFile

        println( output.getAbsolutePath )

        Seq(
          scripts("correct.sh"),
          vectorToString(tl),
          vectorToString(bl),
          vectorToString(br),
          vectorToString(tr),
          width.toString
         ) #< file #> output !

        (output, width / 3)
      }
      case _ => throw new IllegalArgumentException("Invalid point count")
    }
  }

  def perspectiveF(file: File, points: Seq[Vec2])(implicit ec: ExecutionContext, scripts: ScriptsPath): Future[(File, Int)] =
    Future(perspective(file, points))

  def pawnColor(file: File, width: Int, x: Int, y: Int)(implicit ec: ExecutionContext, scripts: ScriptsPath): Option[Boolean] = {
      val cM = (
        Seq(
          scripts("pawn.sh"),
          width.toString,
          ( x * width ).toString,
          ( y * width ).toString
        )  #< file
      ).lineStream.map { str =>
        if( str.contains("#") ){
          val split = str.trim.split(" ")
          Some( (split(1), split(0).toInt) )
        } else None
      }.flatten.toMap[String, Int]

      println(cM.toString)

      ( cM.get("#7F7F7F"), cM.get("#7F7F00"), cM.get("#000000") ) match {
        case ( Some(back), Some(wh), _ ) if  100 * wh / back > 10 => Some(true)
        case ( Some(back), None, Some(bl) ) if 100 * bl / back > 5 => Some(false)
        case ( Some(_), _, _) if cM.size == 1 => None
        case _ => Some(false)
      }
  }

  def pawnColorF(file: File, width: Int, x: Int, y: Int)(implicit ec: ExecutionContext, scripts: ScriptsPath): Future[Option[Boolean]] =
    Future(pawnColor(file, width, x, y))

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
          pawnColorF(gridFile, dimension, x * dimension / 8, y * dimension / 8).map { result =>
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
    val result = for {
      resized <- resize(file)
      positions <- colors(resized)
      corners <- Future(kmeans(positions))
      _ = println(s"Corners: $corners")
      (gridFile, size) <- perspectiveF(resized, corners)
      results <- allPawns(gridFile, size)
    }
    yield results.flatMap { case (pos, valueOpt) =>
      valueOpt.map { value => (pos, value) }
    }.toMap
    result.map { result => println(s"Positions: $result") }
    result
  }
}
