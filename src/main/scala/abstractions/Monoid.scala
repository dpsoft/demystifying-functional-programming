package abstractions

import scala.concurrent.duration.FiniteDuration

object Monoid {
  trait Monoid[A] extends Semigroup.Semigroup[A] {
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
