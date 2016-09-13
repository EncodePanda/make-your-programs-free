package free

import scalaz._, Scalaz._
import scalaz.concurrent.Task
import free.EnrichNTOps._
import free.MAInterpreters._

object UserApp1 extends App {

  type Eff[A] = Coproduct[InOut, ManipulateAccount, A]

  def program[S[_]](implicit
    io: InOut.Ops[S],
    ma: ManipulateAccount.Ops[S]
  ): Free[S, Option[UserAccount]] = for {
    name <- io.ask("What is your login?")
    age <- io.ask("What is your age?")
    h <- ma.create(name, 25)
    user <- ma.fetch(h)
  } yield(user)

  object LiftedConsoleInterpreter extends (InOut ~> StorageState) {
    def apply[A](inout: InOut[A]): StorageState[A] =
      StateT((s: InnerS) => ConsoleInterpreter.apply(inout).map(a => (s, a)))
  }

  val prog = program[Eff]
  val interpreter: Eff ~> StorageState = LiftedConsoleInterpreter :+: MAInterpreters.allInOne
  val result = prog.foldMap(interpreter).eval((0, Map.empty)).unsafePerformSync

  println(result)

}
