package free

import scalaz._, Scalaz._

object LogAndConsole extends App {

  type Eff[A] = Coproduct[InOut, Logging, A]

  val program: Free[Eff, String] = ???

}
