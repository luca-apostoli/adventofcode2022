package Utils

import zio.ZIO
import zio.stream.ZStream


import java.nio.charset.StandardCharsets
import scala.io.BufferedSource
import better.files.File

object Utils {

  def readFile(filename: String, apply: BufferedSource => Iterator[String])
    : ZStream[Any,
    Throwable,
    String] = {
    val path = "src/main/resources/" + filename
    ZStream.fromIteratorScoped(
      ZIO.fromAutoCloseable(
        ZIO.attempt(scala.io.Source.fromFile(path, "UTF-8"))
      ).map(x => apply(x))
    )
  }

  def lines(filename: String): ZStream[Any, Throwable, String] = {
    readFile(filename, _.getLines())
  }

  def raw(filename: String): ZStream[Any, Throwable, String] = {
    val path = "src/main/resources/" + filename
    ZStream.fromIteratorScoped(
        ZIO.attempt(File(path))
      .map(x => x.contentAsString().split('\n').iterator)
    )
  }

  def row(filename: String): ZStream[Any, Throwable, String] = {
    readFile(filename, _.getLines().take(1).flatMap(_.split("")))
  }



}
