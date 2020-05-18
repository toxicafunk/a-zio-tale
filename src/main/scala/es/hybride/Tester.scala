package es.hybride

import zio.{Task, Runtime}
import zio.console._

object Tester {
  val rt = Runtime.unsafeFromLayer(Console.live)

  def parseIntEither(s: String) =
    Task(s.toInt).catchSome{case e:NumberFormatException => {putStrLn(e.getMessage());Task(Int.MinValue)}}

  def main(args: Array[String]): Unit = {
    //val ints  = List("1", "2", "3", "4", "5", "6")
    val ints = List("1", "2", "3", "b", "5", "6")
    val program = Task.foreach(ints)(parseIntEither)
      .flatMap(s => putStrLn(s.toString()))
    rt.unsafeRun(program)
  }
}
