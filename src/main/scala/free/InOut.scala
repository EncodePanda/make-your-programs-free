package free

import scalaz._, Scalaz._
import scalaz.concurrent.Task

sealed trait InOut[A]
case class PrintLine(line: String) extends InOut[Unit]
case object GetLine extends InOut[String]


object InOut {
  def printLine(line: String): Free[InOut, Unit] = Free.liftF(PrintLine(line))
  def getLine(): Free[InOut, String] = Free.liftF(GetLine)
}

object ConsoleInterpreter extends (InOut ~> Task) {

  def apply[A](inout: InOut[A]): Task[A] = inout match {
    case PrintLine(line) => Task.delay {
      println(line)
    }
    case GetLine => Task.delay {
      scala.io.StdIn.readLine()
    }
  }
}

object OurFirstProgram {

  import InOut._

  val program: Free[InOut, Unit] = for {
    _ <- printLine("What is your name")
    name <- getLine()
    _ <- printLine(s"Nice to meet you $name")
  } yield ()
}

object RunInOut extends App {

  val task: Task[Unit] = OurFirstProgram.program.foldMap(ConsoleInterpreter)

  task.unsafePerformSync

}
