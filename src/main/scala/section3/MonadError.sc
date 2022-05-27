import cats._
import cats.implicits._
import java.io.IOException
import scala.util.{Failure, Success, Try}

trait HttpMethod

case object GET extends HttpMethod

case class HttpRequest(method: HttpMethod, url: String)

case class HttpResponse(status: Int)

def doRequest(req: HttpRequest): HttpResponse =
  if (math.random() < 0.5) throw new IOException("boom!")
  else HttpResponse(200)

def executeRequest(req: HttpRequest): Option[HttpResponse] =
  try {
    Some(doRequest(req))
  } catch {
    case _: Exception => None
  }

def executeRequest2(req: HttpRequest): Either[String, HttpResponse] =
  try {
    Right(doRequest(req))
  } catch {
    case _: Exception => Left("Sorry :(")
  }

def executeRequest3(req: HttpRequest): Try[HttpResponse] =
  try {
    Success(doRequest(req))
  } catch {
    case e: Exception => Failure(e)
  }

executeRequest3(HttpRequest(GET, "www.example.com"))


val optionME: MonadError[Option, Unit] = new MonadError[Option, Unit] {

  override def raiseError[A](e: Unit): Option[A] = None

  override def handleErrorWith[A](fa: Option[A])(f: Unit => Option[A]): Option[A] =
    fa.orElse(f(()))

  override def pure[A](x: A): Some[A] = Some(x)

  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

  override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]) = ???
}

def executeRequestME[F[_], E](request: HttpRequest)(f: Exception => E)(implicit ME: MonadError[F, E]): F[HttpResponse] =
  try {
    ME.pure(doRequest(request))
  } catch {
    case e: Exception => ME.raiseError(f(e))
  }

type ErrorOr[A] = Either[Throwable, A]
type ErrorOrS[A] = Either[String, A]
//executeRequestME[ErrorOr](HttpRequest(GET, "www.example.com"))
//executeRequestME[Try](HttpRequest(GET, "www.example.com"))
executeRequestME[Option, Unit](HttpRequest(GET, "www.example.2csom"))((e: Exception) => ())
executeRequestME[ErrorOrS, String](HttpRequest(GET, "www.example.csom"))((e: Exception) => e.getMessage)


MonadError[Option, Unit].attempt(None) // will always return a Some(with Either[L/R] inside)
MonadError[Try, Throwable].attempt(Failure(new Exception("exception"))) // will always return a Success(with Either[L/R] inside)

//MonadError[Option, Unit].ensure(Some(4))(())(_ % 2 == 0)
MonadError[ErrorOrS, String].ensure(Right(5))("oh nos")(_ % 2 == 0)