package abstractions

import scala.language.higherKinds

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    flatMap(ff)(x => map(fa)(x))
}

object MonadExamples extends App {

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa match {
        case None => None
        case Some(a) => f(a)
      }

    def pure[A](a: A): Option[A] = Some(a)
  }

  implicit class OptionMonad[A](opt: Option[A]) {
    def mapFlat[B](f: A => Option[B]): Option[B] = optionMonad.flatMap(opt)(f)
  }
  def half(number: Int): Option[Int] = {
    if (number % 2 == 0) Some(number / 2)
    else None
  }

  val result = Some(6)
    .mapFlat(half) // Some(3)
    .mapFlat(half) // None
    .mapFlat(half) // None

  println(s"RESULT: $result")

}
