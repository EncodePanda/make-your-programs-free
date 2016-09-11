package free

import scalaz._, Scalaz._

sealed trait InOut[A]
case class PrintLine(line: String) extends InOut[Unit]
case object GetLine extends InOut[String]

