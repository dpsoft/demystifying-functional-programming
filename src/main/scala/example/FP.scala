package example

import cats.data.EitherT
import cats.implicits._
import cats.{Id, Monad}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.{higherKinds, postfixOps}


case class User(id: Long, email: String, loyaltyPoints: Int)

// Initial code, with no MonadT
object V0 {
  trait UserStore {
    def findUser(id: Long): Future[Either[Throwable, User]]
    def canBeUpdated(user: User): Future[Either[Throwable, Boolean]]
    def updateUser(user: User)(pointsToAdd: Int): Future[Either[Throwable, User]]
  }

  class LoyaltyPointsV00(store: UserStore) {
    def addPoints(userId: Long, pointsToAdd: Int): Future[Either[Throwable, User]] = {
      val result: Future[Either[Throwable, User]] = for {
        user        <- store.findUser(userId)
        _           <- user.right.map(store.canBeUpdated).left.map(Future.failed).merge
        updatedUser <- user.right.map(store.updateUser(_)(pointsToAdd)).left.map(Future.failed).merge
      } yield updatedUser
      result
    }
  }
}


// Introduce EitherT
object V1 {
  trait UserStore {
    def findUser(id: Long): Future[Either[Throwable, User]]
    def canBeUpdated(user: User): Future[Either[Throwable, Boolean]]
    def updateUser(user: User)(pointsToAdd: Int): Future[Either[Throwable, User]]
  }

  class LoyaltyPointsV0(store: UserStore) {
    def addPoints(userId: Long, pointsToAdd: Int): Future[Either[Throwable, User]] = {
      val result: EitherT[Future, Throwable, User] = for {
        user        <- EitherT(store.findUser(userId))
        _           <- EitherT(store.canBeUpdated(user))
        updatedUser <- EitherT(store.updateUser(user)(pointsToAdd))
      } yield updatedUser

      result.value // EitherT[Future, Throwable, User] => Future[Either[Throwable, User]]
    }
  }
}

// Extract description from interpretation
object V2 extends App {

  trait UserStoreAlg[F[_]] {
    def findUser(id: Long): F[Either[Throwable, User]]
    def canBeUpdated(user: User): F[Either[Throwable, Boolean]]
    def updateUser(user: User)(pointsToAdd: Int): F[Either[Throwable, User]]
  }

  class LoyaltyPointsV1[F[_]](store: UserStoreAlg[F])(implicit M: Monad[F]) {
    def addPoints(userId: Long, pointsToAdd: Int): F[Either[Throwable, User]] = {
      val result: EitherT[F, Throwable, User] = for {
        user        <- EitherT(store.findUser(userId))
        _           <- EitherT(store.canBeUpdated(user))
        updatedUser <- EitherT(store.updateUser(user)(pointsToAdd))
      } yield updatedUser

      result.value // EitherT[F, Throwable, User] => F[Either[Throwable, User]]
    }
  }

  trait FutureInterpreter extends UserStoreAlg[Future] {
    override def findUser(id: Long): Future[Either[Throwable, User]] =
      Future.successful(Right(User(1L, "awesome@fp.com", 1000)))

    override def canBeUpdated(user: User): Future[Either[Throwable, Boolean]] =
      Future.successful(Right(true))

    override def updateUser(user: User)(pointsToAdd: Int): Future[Either[Throwable, User]] =
      Future.successful(Right(user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)))
  }

  trait IdInterpreter extends UserStoreAlg[Id] {
    override def findUser(id: Long): Id[Either[Throwable, User]] =
      Right(User(1L, "awesome@fp.com", 1000))

    override def canBeUpdated(user: User): Id[Either[Throwable, Boolean]] =
      Right(true)

    override def updateUser(user: User)(pointsToAdd: Int): Id[Either[Throwable, User]] =
      Right(user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd))
  }

  object FutureInterpreter extends FutureInterpreter

  private val interpreter = new FutureInterpreter {}
  val result: Future[Either[Throwable, User]] = new LoyaltyPointsV1(interpreter).addPoints(1, 100)

  Await.result(result, 1 second).foreach(println)

}

// Final refactor
object V3 extends App{

  case class ServiceError(msg: String)

  type ResultOrError[F[_], A] = EitherT[F, ServiceError, A]

  trait UserStoreAlgV2[F[_]] {
    def findUser(id: Long): ResultOrError[F, User]
    def canBeUpdated(user: User): ResultOrError[F, Boolean]
    def updateUser(user: User)(pointsToAdd: Int): ResultOrError[F, User]
  }


  class LoyaltyPointsV2[F[_]](store: UserStoreAlgV2[F])(implicit M: Monad[F]) {
    def addPoints(userId: Long, pointsToAdd: Int): F[Either[ServiceError, User]] = {
      val result: ResultOrError[F, User] = for {
        user        <- store.findUser(userId)
        _           <- store.canBeUpdated(user)
        updatedUser <- store.updateUser(user)(pointsToAdd)
      } yield updatedUser
      result.value // EitherT[F, ServiceError, User] => F[Either[ServiceError, User]]
    }
  }

  trait FutureInterpreter extends UserStoreAlgV2[Future] {
    override def findUser(id: Long): EitherT[Future, ServiceError, User] =
      EitherT(Future.successful(Either.right(User(1L, "awesome@fp.com", 1000))))

    override def canBeUpdated(user: User): ResultOrError[Future, Boolean] =
      EitherT(Future.successful(Either.right(true)))

    override def updateUser(user: User)(pointsToAdd: Int): ResultOrError[Future, User] =
      EitherT(Future.successful(Either.right(user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd))))
  }

  object FutureInterpreter extends FutureInterpreter

  val result: Future[Either[ServiceError, User]] = new LoyaltyPointsV2(new FutureInterpreter{}).addPoints(1, 100)

  Await.result(result, 1 second).foreach(println)
}
