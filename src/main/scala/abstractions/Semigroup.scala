package abstractions

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
