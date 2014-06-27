package turkotron

import play.api.Play
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import scala.concurrent._
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

class ImageAnalyzeSpec extends Specification {

  private val initial = Game.initialPosition

  val serverPath = "/Volumes/Dev/work/dev/hackday/turkotron/server"

  implicit val path = ImageToPosition.ScriptsPath(serverPath + "/scripts")

  "ImageToPosition" should {

    "works with a mock image" in {
      val expected = Map.empty[String, Boolean]
      val result = Await.result(ImageToPosition(new java.io.File(serverPath + "/test/IMG_20140627_104247.jpg")), DurationInt(20).seconds)

      result == expected
    }
  }

  "ImageToPosition" should {

    "works with a mock image" in {
      val file = new java.io.File(serverPath + "/test/input2.jpg")

      val (output, width) = ImageToPosition.perspective(file, Seq(
        Seq(222, 274),
        Seq(160, 1681),
        Seq(1615, 1713),
        Seq(1584, 225)
      ))

      println("0-0 : " + ImageToPosition.pawnColor(output, width, 0, 0).toString )
      println("1-0 : " + ImageToPosition.pawnColor(output, width, 1, 0).toString )
      println("2-0 : " + ImageToPosition.pawnColor(output, width, 2, 0).toString )

      println("0-1 : " + ImageToPosition.pawnColor(output, width, 0, 1).toString )
      println("1-1 : " + ImageToPosition.pawnColor(output, width, 1, 1).toString )
      println("2-1 : " + ImageToPosition.pawnColor(output, width, 2, 1).toString )

      println("0-2 : " + ImageToPosition.pawnColor(output, width, 0, 2).toString )
      println("1-2 : " + ImageToPosition.pawnColor(output, width, 1, 2).toString )
      println("2-2 : " + ImageToPosition.pawnColor(output, width, 2, 2).toString )

      true
    }
  }

}

