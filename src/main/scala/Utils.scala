import scala.io.Source

object Utils {
  def read(filename: String): Iterator[String] = {
    Source.fromResource(filename).getLines
  }

}
