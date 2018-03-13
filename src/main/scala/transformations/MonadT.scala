package transformations

import cats.data.{EitherT, OptionT}
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.{higherKinds, postfixOps}

object OptionTExample extends App {
  val greetingFutureOption: Future[Option[String]] = Future.successful(Some("Hello"))
  val firstnameFuture: Future[String] = Future.successful("Jane")
  val lastnameOption: Option[String] = Some("Doe")


  val a: Future[Option[String]] = (for {
    g <- OptionT(greetingFutureOption)
    f <- OptionT.liftF(firstnameFuture)
    l <- OptionT.fromOption[Future](lastnameOption)
  } yield s"$g $f $l").value

  Await.result(a, 1 second).foreach(println)
}


object Compose extends App {
  val greetingOption: Option[String] = Some("Hello")
  val firstNameOption: Option[String] = Some("John")
  val lastNameOption: Option[String] = Some("Doe")

  for {
    g <- greetingOption
    f <- firstNameOption
    l <- lastNameOption
  } yield s"$g $f $l"
}

object EitherTExample extends App {
  import cats.implicits._

  val greetingFutureEither: Future[Either[Throwable, String]] = Future.successful(Either.right("Hello"))
  val firstNameFuture: Future[String] = Future.successful("Jane")
  val lastNameOption: Option[String] = Some("Doe")

  def greet: Future[Either[Throwable, String]] = {
    val result: EitherT[Future, Throwable, String] = for {
      g <- EitherT(greetingFutureEither)
      f <- EitherT.liftF(firstNameFuture) // => Right(firstNameFuture)
      l <- EitherT.fromOption[Future](lastNameOption, new Throwable("lastName not found"))// => EitherT(Future(Right(lastNameOption)))
    } yield s"$g $f $l"

    result.value
  }
}

object StackThemAll {
  case class User(id:String)

  case class AwesomeError(msg: String)

  type ResultT[F[_], A] = EitherT[F, AwesomeError, A]
  type FutureResult[A] = ResultT[Future, A]

  def getUser(id: String): FutureResult[User] = ???
  def canBeUpdated(user: User): FutureResult[User] = ???
  def update(user: User): FutureResult[User] = ???


  def updateUser(user: User):FutureResult[User] = for {
    u           <- getUser(user.id)
    _           <- canBeUpdated(u)
    updatedUser <- updateUser(u)
  } yield updatedUser
}
