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

  case class Ops[S[_]](implicit s0: Logging :<: S) {
    def info(line: String): Free[S, Unit] = Free.liftF(s0.inj(Info(line)))
    def warn(line: String): Free[S, Unit] = Free.liftF(s0.inj(Warn(line)))
    def error(line: String): Free[S, Unit] = Free.liftF(s0.inj(Error(line)))
    def debug(line: String): Free[S, Unit] = Free.liftF(s0.inj(Debug(line)))
  }
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

  implicit val ops = new Logging.Ops[Logging]()

  val program = for {
    _ <- ops.info("starting application!")
    _ <- ops.debug("omg, app is running!")
  } yield()

  val task: Task[Unit] = program.foldMap(Interpreter)

  task.unsafePerformSync
}
