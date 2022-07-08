import cats._
import cats.implicits._
import cats.data._

trait AccountRepo
type ErrorOr[A] = Either[String, A]
type AccountOp[A] = ReaderT[ErrorOr, AccountRepo, A]
val dummyRepo: AccountRepo = new AccountRepo {}

// ReaderT == Kleisli

// ErrorOr[B] -> ReaderT[ErrorOr, A, B]
ReaderT.liftF[ErrorOr, AccountRepo, Int](5.asRight[String]).run(dummyRepo)
ReaderT.liftF[ErrorOr, AccountRepo, Int]("hello".asLeft[Int]).run(dummyRepo)

// B -> ReaderT[ErrorOr, A, B]
5.pure[AccountOp].run(dummyRepo)

// ReaderT[ErrorOr, A, B] -> ReaderT[Option, A, B]
5.pure[AccountOp]
  .mapF {
    case Right(x) => Some(x)
    case Left(s)  => None
  }
  .run(dummyRepo)

ReaderT((_: AccountRepo) => 5.asRight[String]).run(dummyRepo)
ReaderT((_: AccountRepo) => "hello".asLeft[Int]).run(dummyRepo)

5.pure[AccountOp].map(_ + 1).run(dummyRepo)
ReaderT((_: AccountRepo) => "hello".asLeft[Int]).map(_ + 1).run(dummyRepo)

5.pure[AccountOp].flatMap(n => (n + 1).pure[AccountOp]).run(dummyRepo)
ReaderT((_: AccountRepo) => "hello".asLeft[Int])
  .flatMap(n => (n + 1).pure[AccountOp])
  .run(dummyRepo)
