package free

import scalaz._, Scalaz._
import scalaz.concurrent.Task

sealed trait InOut[A]
case class PrintLine(line: String) extends InOut[Unit]
case object GetLine extends InOut[String]


object InOut {

  class Ops[S[_]](implicit s0: InOut :<: S) {

    def printLine(line: String): Free[S, Unit] = Free.liftF(s0.inj(PrintLine(line)))
    def getLine(): Free[S, String] = Free.liftF(s0.inj(GetLine))

    def ask(question: String): Free[S, String] = for {
      _ <- printLine(question)
      answer <- getLine()
    } yield answer
  }

  object Ops {
    implicit def apply[S[_]](implicit S: InOut :<: S): Ops[S] =
      new Ops[S]
  }
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

object InOutInterpreter {

  def injectFT[F[_], S[_]](implicit S: F :<: S): F ~> Free[S, ?] =
    liftFT[S] compose injectNT[F, S]

  def liftFT[S[_]]: S ~> Free[S, ?] =
    new (S ~> Free[S, ?]) {
      def apply[A](s: S[A]) = Free.liftF(s)
    }

  def injectNT[F[_], G[_]](implicit I: F :<: G): F ~> G =
    new (F ~> G) {
      def apply[A](fa: F[A]) = I inj fa
    }


  def interpreter[S[_]](implicit s0: Task :<: S) = new (InOut ~> Free[S, ?]) {
    def apply[A](inout: InOut[A]): Free[S, A] = inout match {
      case PrintLine(line) => injectFT.apply(Task.delay {
        println(line)
      })
      case GetLine => injectFT.apply(Task.delay {
        scala.io.StdIn.readLine()
      })
    }
  }
}

object OurFirstProgram {
  def program[S[_]](implicit io: InOut.Ops[InOut]): Free[InOut, Unit] = for {
    name <- io.ask("What is your name")
    _ <- io.printLine(s"Nice to meet you $name")
  } yield ()
}

object RunInOut extends App {

  implicit val ops = new InOut.Ops[InOut]
  val task: Task[Unit] = OurFirstProgram.program.foldMap(ConsoleInterpreter)
  task.unsafePerformSync

}
