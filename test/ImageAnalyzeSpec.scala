package turkotron

import play.api.Play
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import scala.concurrent.ExecutionContext.Implicits.global

class ImageAnalyzeSpec extends Specification {

  private val initial = Game.initialPosition

  implicit val path = ImageToPosition.ScriptsPath("/Users/gre/Desktop/chess/server/scripts")

  "ImageToPosition" should {

    "works with a mock image" in {

      ImageToPosition.apply(new java.io.File("/Users/gre/Desktop/chess/server/test/IMG_20140627_104247.jpg"))
      
    }
  }
}

