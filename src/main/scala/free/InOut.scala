package free

import scalaz._, Scalaz._

sealed trait InOut[A]
case class PrintLine(line: String) extends InOut[Unit]
case object GetLine extends InOut[String]


object InOut {
  def printLine(line: String): Free[InOut, Unit] = Free.liftF(PrintLine(line))
  def getLine(): Free[InOut, String] = Free.liftF(GetLine)
}

