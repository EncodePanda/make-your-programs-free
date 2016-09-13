package free

import scalaz._, Scalaz._
import scalaz.concurrent.Task

sealed trait KVS[K, V, A]

object KVS {

  final case class Get[K, V](k: K) extends KVS[K, V, Option[V]]
  final case class Put[K, V](k: K, v: V) extends KVS[K, V, Unit]
  final case class Delete[K, V](k: K) extends KVS[K, V, Unit]

  class Ops[S[_], K, V](implicit s0: KVS[K, V, ?] :<: S) {

    def get(key: K): Free[S, Option[V]] = lift[S, K,V, Option[V]](Get(key))
    def put(key: K, value: V): Free[S, Unit] = lift[S, K, V, Unit](Put(key, value))
    def delete(key: K): Free[S, Unit] = lift[S, K, V, Unit](Delete(key))

  }

  object Ops {
    implicit def apply[S[_], K, V](implicit S: KVS[K, V, ?] :<: S): Ops[S, K, V] =
      new Ops[S, K, V]
  }

  private def lift[S[_], K, V, A](kvs: KVS[K, V, A])(implicit s0: KVS[K,V,?] :<: S) =
    Free.liftF(s0.inj(kvs))
}

object KVSInterpreters {

  import KVS._

  def fromMapState[K, V] = new (KVS[K, V, ?] ~> State[Map[K, V], ?]) {
    def apply[A](kvs: KVS[K, V, A]): State[Map[K, V], A] = kvs match {
      case Get(k) => State {
        case m => (m, m.get(k))
      }
      case Put(k, v) => State {
        case m => (m + (k -> v), ())
      }
      case Delete(k) => State {
        case m => (m - k, ())
      }
    }
  }
}
