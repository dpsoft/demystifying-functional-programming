import cats.{Id, Monad}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.{EitherT, OptionT}
import cats.implicits._

import scala.concurrent.duration._


case class Box[T](value:T)

object TypeClass {
  // The type class itself is a trait with a single type parameter.
  trait HtmlWriter[A] {
    def write(value: A): String
  }

  //Type class instances
  implicit val intWriter:HtmlWriter[Int] =
    new HtmlWriter[Int] {
      override def write(value: Int): String =
        value.toString
    }

  implicit val stringWriter:HtmlWriter[String] =
    new HtmlWriter[String] {
      override def write(value: String): String =
        value.replaceAll("<", "&lt;").replaceAll(">", "&gt;")
    }

  //Extra type class instances
  implicit def boxWriter[A]:HtmlWriter[Box[A]] =
    new HtmlWriter[Box[A]] {
      override def write(value: Box[A]): String = ???
    }

  def toHtml[A](value: A)(implicit writer: HtmlWriter[A]): String =
    writer.write(value)
}


object Semigroup {
  trait Semigroup[A] {
    /**
      * Associativity means:
      *
      * combine(x, combine(y, z)) = combine(combine(x, y), z)
      */
    def append(x: A, y: A): A //combine | compose | whatever
  }

  implicit val intAdditionSemigroup: Semigroup[Int] =
    new Semigroup[Int] {
      override def append(x: Int, y: Int): Int = x + y
  }

  implicit def listCombineSemigroup[A]: Semigroup[List[A]] =
    new Semigroup[List[A]] {
      override def append(x: List[A], y: List[A]): List[A] = x ++ y
    }

  def append[A](x: A, y: A)(implicit semigroup: Semigroup[A]): A =
    semigroup.append(x, y)

  def combineAll[A](x: A, y: A)(implicit semigroup: Semigroup[A]): A =
    semigroup.append(x, y)
}

object Monoid {
  trait Monoid[A] extends Semigroup[A] {
    /**
      * Identity means:
      *
      * combine(x, empty) = combine(empty, x) = x
      */
    def empty: A
  }

  implicit val intMonoid:Monoid[Int] =
    new Monoid[Int] {
      def append(x: Int, y: Int): Int =  x + y
      def empty: Int = 0
    }

  def combineAll[A](as: List[A])(implicit monoid: Monoid[A]): A =
    as.foldLeft(monoid.empty)(monoid.append)

  implicit val timesMonoid: Monoid[Times] = new Monoid[Times] {
    def append(x: Times, y: Times): Times = Times(x.data ++ y.data)
    def empty: Times = Times(Map.empty)
  }


  def reduce[A](as: List[A])(implicit monoid: Monoid[A]): A =
    as.foldLeft(monoid.empty)(monoid.append)

  case class Times(data: Map[String, FiniteDuration])

}


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

object Error {
  case class User(id:String)

  case class AwesomeError(msg: String)

  type ResultT[F[_], A] = EitherT[F, AwesomeError, A]
  type FutureResult[A] = ResultT[Future, A]

  def getUser (id:String):FutureResult[User] = ???
  def canBeUpdated (user:User):FutureResult[User] = ???
  def update (user:User):FutureResult[User] = ???


  def updateUser(user: User):FutureResult[User] = for {
    u <- getUser(user.id)
    _ <- canBeUpdated(u)
    updatedUser <- updateUser(u)
  } yield updatedUser
}


case class User(id: Long, email: String, loyaltyPoints: Int)

object V00 {
  trait UserStore {
    def findUser(id: Long): Future[Either[Throwable, User]]
    def canBeUpdated(user: User): Future[Either[Throwable, Boolean]]
    def updateUser(user: User)(pointsToAdd: Int): Future[Either[Throwable, User]]
  }

  class LoyaltyPointsV00(store: UserStore) {
    def addPoints(userId: Long, pointsToAdd: Int): Future[Either[Throwable, User]] = {
      val result: Future[Either[Throwable, User]] = for {
        user <- store.findUser(userId)
        _ <- user.right.map(store.canBeUpdated).left.map(Future.failed).merge
        updatedUser <- user.right.map(store.updateUser(_)(pointsToAdd)).left.map(Future.failed).merge
      } yield updatedUser
      result
    }
  }
}


object V0 {
  trait UserStore {
    def findUser(id: Long): Future[Either[Throwable, User]]
    def canBeUpdated(user: User): Future[Either[Throwable, Boolean]]
    def updateUser(user: User)(pointsToAdd: Int): Future[Either[Throwable, User]]
  }

  class LoyaltyPointsV0(store: UserStore) {
    def addPoints(userId: Long, pointsToAdd: Int): Future[Either[Throwable, User]] = {
      val result: EitherT[Future, Throwable, User] = for {
        user <- EitherT(store.findUser(userId))
        _ <- EitherT(store.canBeUpdated(user))
        updatedUser <- EitherT(store.updateUser(user)(pointsToAdd))
      } yield updatedUser

      result.value // EitherT[Future, Throwable, User] => Future[Either[Throwable, User]]
    }
  }
}

object V1 extends App {

  trait UserStoreAlg[F[_]] {
    def findUser(id: Long): F[Either[Throwable, User]]
    def canBeUpdated(user: User): F[Either[Throwable, Boolean]]
    def updateUser(user: User)(pointsToAdd: Int): F[Either[Throwable, User]]
  }

  class LoyaltyPointsV1[F[_]](store: UserStoreAlg[F])(implicit M: Monad[F]) {
    def addPoints(userId: Long, pointsToAdd: Int): F[Either[Throwable, User]] = {
      val result: EitherT[F, Throwable, User] = for {
        user <- EitherT(store.findUser(userId))
        _ <- EitherT(store.canBeUpdated(user))
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

//Primer Refactor
object V2 extends App{

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
        user <- store.findUser(userId)
        _ <- store.canBeUpdated(user)
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
