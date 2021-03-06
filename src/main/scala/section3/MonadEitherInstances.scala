package section3

import cats._
import cats.implicits._

import scala.util.{Failure, Success, Try}

// Either[E, *] : * -> *
object MonadEitherInstances {

  def eitherME[E]: MonadError[Either[E, *], E] = new MonadError[Either[E, *], E] {
    override def raiseError[A](e: E): Either[E, A] =
      e.asLeft

    override def handleErrorWith[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] =
      fa match {
        case Left(e) => f(e)
        case right => right
      }

    override def pure[A](x: A): Either[E, A] = ???

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ???

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???
  }

  def tryMe[E]: MonadError[Try, Throwable] = new MonadError[Try, Throwable]  {
    override def raiseError[A](e: Throwable): Try[A] = Failure(e)

    override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] =
      fa match {
        case Success(a) => Success(a)
        case Failure(t) => f(t)
      }

    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = ???

    override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???

    override def pure[A](x: A): Try[A] = ???
  }



}
