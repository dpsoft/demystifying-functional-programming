package abstractions

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  def pure[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)

}

object ApplicativeExamples extends App {

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = (ff, fa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case _ => None
    }

    override def pure[A](a: A): Option[A] = Option(a)
  }

  def apOption[A, B](opt: Option[A])
                    (f: Option[A => B])
                    (implicit functor: Applicative[Option]): Option[B] =
    optionApplicative.ap(f)(opt)

  println("Executing Applicative examples. Validating assertions...")

  assert(apOption[Int, String](Some(2))(Some(i => s"The number is: ${i.toString}")).contains("The number is: 2"))
  assert(apOption[Int, String](None)(Some(i => s"The number is: ${i.toString}")).isEmpty)
  assert(apOption(Some(2))(None).isEmpty)
}
