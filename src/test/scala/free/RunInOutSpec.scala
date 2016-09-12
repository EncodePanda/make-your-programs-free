package free

import org.scalatest._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

import scalaz._, Scalaz._

class RunInOutSpec extends FreeSpec with Matchers {

  def interpreter(input: Stack[String], output: ListBuffer[String]) = new (InOut ~> Id) {

    def apply[A](inout: InOut[A]): Id[A] = inout match {
      case PrintLine(line) =>
        output += line
        ()
      case GetLine =>
        input.pop
    }
  }


  "A program" - {
    "should ask for a name and greet the user" in {
      // given
      val input = Stack.empty[String]
      val output = ListBuffer.empty[String]
      input.push("Pawel")
      // when
      OurFirstProgram.program.foldMap(interpreter(input, output))
      // then
      input.size should be(0)
      output should equal(ListBuffer("What is your name", "Nice to meet you Pawel"))
    }
  }
}
