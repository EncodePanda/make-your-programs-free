package free

import scalaz._, Scalaz._
import scalaz.concurrent.Task

sealed trait MonotonicSeq[A]

object MonotonicSeq {
  case class Next() extends MonotonicSeq[Long]

  class Ops[S[_]](implicit s0: MonotonicSeq :<: S) {
    def next: Free[S, Long] = Free.liftF(s0.inj(Next()))
  }

  object Ops {
    implicit def apply[S[_]](implicit S: MonotonicSeq :<: S): Ops[S] =
      new Ops[S]
  }

}

object MSInterpreters {
  import MonotonicSeq._

  def fromMapState[K, V] = new (MonotonicSeq ~> Id) {
    def apply[A](ms: MonotonicSeq[A]): Id[A] = ms match {
      case Next() => System.nanoTime
    }
  }
}
