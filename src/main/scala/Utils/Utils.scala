package Utils

import zio.ZIO
import zio.stream.ZStream

object Utils {

  def lines(filename: String): ZStream[Any, Throwable, String] = {
    val path = "src/main/resources/" + filename
    ZStream.fromIteratorScoped(
      ZIO.fromAutoCloseable(
        ZIO.attempt(scala.io.Source.fromFile(path))
      ).map(_.getLines())
    )
  }

}
