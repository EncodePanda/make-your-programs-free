package free

import scalaz._, Scalaz._
import scalaz.concurrent.Task
import free.EnrichNTOps._

object LogAndConsole extends App {

  type Eff[A] = Coproduct[InOut, Logging, A]

  def program[S[_]](implicit
    io: InOut.Ops[S],
    log: Logging.Ops[S]
  ): Free[S, String] = for {
    _ <- log.info("Program started")
    name <- io.ask("What is your name?")
    _ <- log.debug(s"The name was $name")
  } yield(name)

  val prog = program[Eff]
  val interpreter: Eff ~> Task = ConsoleInterpreter :+: Log4JInterpreter
  prog.foldMap(interpreter).unsafePerformSync

}
