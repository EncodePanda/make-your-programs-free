package free

import scalaz._, Scalaz._
import scalaz.concurrent.Task
import org.slf4j.LoggerFactory

sealed trait Logging[A]
case class Info(line: String) extends Logging[Unit]
case class Warn(line: String) extends Logging[Unit]
case class Error(line: String) extends Logging[Unit]
case class Debug(line: String) extends Logging[Unit]

object Logging {
  def info(line: String): Free[Logging, Unit] = Free.liftF(Info(line))
  def warn(line: String): Free[Logging, Unit] = Free.liftF(Warn(line))
  def error(line: String): Free[Logging, Unit] = Free.liftF(Error(line))
  def debug(line: String): Free[Logging, Unit] = Free.liftF(Debug(line))
}

object Interpreter extends (Logging ~> Task) {
  def apply[A](inout: Logging[A]): Task[A] = inout match {
    case Info(line) => Task.delay {
      LoggerFactory.getLogger(this.getClass).info(line)
    }
    case Error(line) => Task.delay {
      LoggerFactory.getLogger(this.getClass).error(line)
    }
    case Warn(line) => Task.delay {
      LoggerFactory.getLogger(this.getClass).warn(line)
    }
    case Debug(line) => Task.delay {
      LoggerFactory.getLogger(this.getClass).debug(line)
    }
  }
}

object RunLogging extends App {

  import Logging._

  val program = for {
    _ <- info("starting application!")
    _ <- debug("omg, app is running!")
  } yield()

  val task: Task[Unit] = program.foldMap(Interpreter)

  task.unsafePerformSync
}
