package free


import scalaz._, Scalaz._
import scalaz.concurrent.Task

case class UserAccount(login: String, age: Int, active: Boolean = true)

sealed trait ManipulateAccount[A]

object ManipulateAccount {

  type Handler = Long

  case class CreateAccount(login: String, age: Int) extends ManipulateAccount[Handler]
  case class UpdateAge(handler: Handler, age: Int) extends ManipulateAccount[Unit]
  case class DeactivateAccount(handler: Handler) extends ManipulateAccount[Unit]
  case class DeleteAccount(handler: Handler) extends ManipulateAccount[Unit]
  case class FetchAccount(handler: Handler) extends ManipulateAccount[Option[UserAccount]]

  class Ops[S[_]](implicit s0: ManipulateAccount :<: S) {
    def create(login: String, age: Int) = Free.liftF(s0.inj(CreateAccount(login, age)))
    def updateAge(handler: Handler, age: Int) = Free.liftF(s0.inj(UpdateAge(handler, age)))
    def deactivate(handler: Handler) = Free.liftF(s0.inj(DeactivateAccount(handler)))
    def deleteAccount(handler: Handler) = Free.liftF(s0.inj(DeactivateAccount(handler)))
    def fetch(handler: Handler) = Free.liftF(s0.inj(FetchAccount(handler)))
  }

  object Ops {
    implicit def apply[S[_]](implicit S: ManipulateAccount :<: S): Ops[S] =
      new Ops[S]
  }

}

object MAInterpreters {

  import ManipulateAccount._

  type InnerS = (Long,Map[Handler, UserAccount])
  type StorageState[A] = StateT[Task, InnerS, A]

  def allInOne =  new (ManipulateAccount ~> StorageState) {
    def apply[A](ma: ManipulateAccount[A]): StorageState[A] = ma match {
      case CreateAccount(l, a) => StateT[Task, InnerS, Handler] {
        case (idx, m) => {
          val handler = idx + 1
          val newState = (handler, m + (handler -> UserAccount(l,a)))
          (newState, handler).point[Task]
        }
      }
      case UpdateAge(h, a) => StateT[Task, InnerS, Unit] {
        case (idx, m) => {
          m.get(h).map {
            case userAccount => userAccount.copy(age = a)
          }.fold(((idx, m), ()))(u => ((idx, m + (h -> u)), ())).point[Task]
        }
      }
      case DeactivateAccount(h) => StateT[Task, InnerS, Unit] {
        case (idx, m) => {
          m.get(h).map {
            case userAccount => userAccount.copy(active = false)
          }.fold(((idx, m), ()))(u => ((idx, m + (h -> u)), ())).point[Task]
        }
      }
      case DeleteAccount(h) => StateT[Task, InnerS, Unit] {
        case (idx, m) => ((idx, m - h), ()).point[Task]
      }
      case FetchAccount(h) => StateT[Task, InnerS, Option[UserAccount]] {
        case (idx, m) => ((idx, m), m.get(h)).point[Task]
      }
    }
  }
}


object ManipulateAccountRun extends App {

  import MAInterpreters._

  def program[S[_]](implicit ma: ManipulateAccount.Ops[S]) = for {
    handler <- ma.create("pawel", 32)
    _ <- ma.updateAge(handler, 26)
    _ <- ma.deactivate(handler)
    maybeUser <- ma.fetch(handler)
  } yield(maybeUser)

  val storage: StorageState[Option[UserAccount]] =
    program[ManipulateAccount].foldMap(allInOne)

  println(storage.eval((0, Map.empty)).unsafePerformSync)
}
