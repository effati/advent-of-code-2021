import scala.io.Source

object Utils {
  def read(filename: String): List[String] = {
    Source.fromResource(filename).getLines.toList
  }
}
