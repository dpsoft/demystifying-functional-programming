package typeclasses

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
