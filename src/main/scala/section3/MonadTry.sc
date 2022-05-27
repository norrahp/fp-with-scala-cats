import cats._
import cats.implicits._
import scala.util._
//
//implicit  val tryMonad: Monad[Try] = new Monad[Try] {
//  override def pure[A](x: A): Try[A] = Success(x)
//
//  override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] =
//    fa match {
//      case Success(a) => f(a)
//      case Failure(ex) => Failure(ex)
//    }
//
//  override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???
//
//}
//
//tryMonad.pure(5)
//tryMonad.pure(5).flatMap(i => tryMonad.pure(i + 1))
//tryMonad.pure(5).flatMap(i => Failure(new Exception("boom")))
//
//// fail fast
//tryMonad.pure(5)
//  .flatMap((i: Int) => Failure(new Exception("boom"))
//  .flatMap((j: Int) => Failure(new Exception("boom 2"))))
//

Success(5).flatMap(i => throw new Exception("boom"))
