package free

import scalaz._, Scalaz._

object LogAndConsole extends App {

  type Eff[A] = Coproduct[InOut, Logging, A]

  def program: Free[Eff, String] = ???

}
