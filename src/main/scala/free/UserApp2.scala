package free

import scalaz._, Scalaz._
import scalaz.concurrent.Task
import free.EnrichNTOps._

object UserApp2 extends App {
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

  val prog = program[Eff]

  def interpreter[S[_]](implicit
    s0: KVS[Long, UserAccount, ?] :<: S,
    s1: MonotonicSeq :<: S,
    s2: Task :<: S
  ): Eff ~> Free[S, ?] = {
    type G[A] = Free[S, A]

    val first: InOut ~> G = InOutInterpreter.interpreter[S]
    val second: ManipulateAccount ~> G =  MAInterpreters.interpreter[S]

    first :+: second
  }


}
