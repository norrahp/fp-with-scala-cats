package section3

import cats._
import cats.implicits._

case class Box[A](value: A)

object Box {
  // Implement an instance of Eq[Box[A]] for any A that has an Eq instance
  implicit def eqBox[A](implicit eqA: Eq[A]): Eq[Box[A]] = Eq.by(_.value)

  // Implement an instance of Monad[Box]
  implicit val boxMonad = new Monad[Box] {
    override def pure[A](x: A): Box[A] = Box(x)

    override def flatMap[A, B](fa: Box[A])(f: A => Box[B]): Box[B] = {
      f(fa.value)
    }

    override def tailRecM[A, B](a: A)(f: A => Box[Either[A, B]]): Box[B] =
      f(a).value match {
        case Right(b) => Box(b)
        case Left(a) => tailRecM(a)(f)
      }
  }
}

