package abstractions

import scala.language.higherKinds

trait Functor[F[_]] {
  /**
    * Identity means: Option(identity(1)) == identity(Option(1))
    * Composition means:
    *     `F.map(Some(3))(x => f.andThen(g)(x))`
    *   is equivalent to:
    *     `F.map( F.map(Some(3))(f) )(g)`
    */
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))

  // also called `unzip`
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  // also called `zip`
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Right(fb) => map(fb)(Right.apply)
    case Left(fa) => map(fa)(Left.apply)
  }
}

object FunctorExamples extends App {

  implicit val optionFunctor: Functor[Option] =
    new Functor[Option] {
      def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
        case None => None
        case Some(a) => Some(f(a))
      }
    }

  implicit val listFunctor: Functor[List] =
    new Functor[List] {
      def map[A, B](fa: List[A])(f: A => B): List[B] = fa match {
        case Nil => Nil
        case a :: as => f(a) :: map(as)(f)
      }
    }

  def mapOption[A, B](as: Option[A])
                     (f: A => B)
                     (implicit functor: Functor[Option]): Option[B] =
    functor.map(as)(f)

  println("Executing Functor examples. Validating assertions...")

  assert(listFunctor.map(List("Paul", "Ng", "Sam"))(_.length > 2) == List(true, false, true))

  assert(mapOption(Some(5))(_ + 3).contains(8))
  assert(mapOption(Option.empty[Int])(_ + 3).isEmpty)

  assert(listFunctor.map(List(2, 8, 12))(_ + 3) == List(5, 11, 15))

}
