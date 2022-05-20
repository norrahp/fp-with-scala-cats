import cats.Functor

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

class Secret[A](val value: A) {
  private def hashed: String = {
    val s = value.toString
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    val d = MessageDigest.getInstance("SHA-1")
    val hashedBytes = d.digest(bytes)
    new String(hashedBytes, StandardCharsets.UTF_8)
  }

  override def toString: String = hashed
}

object Secret {
  implicit val secretFunctor = new Functor[Secret] {
    override def map[A, B](fa: Secret[A])(f: A => B) : Secret[B] =
      new Secret(f(fa.value))
  }
}

val philSecret: Secret[String] = new Secret[String]("Phil")
philSecret.value

val upperPhilSecret = Functor[Secret].map(philSecret)(_.toUpperCase)
upperPhilSecret.value

val optionFunctor: Functor[Option] = new Functor[Option] {
  override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
    fa match {
      case None => None
      case Some(a) => Some(f(a))
    }
}

val listFunctor: Functor[List] = new Functor[List] {
  override def map[A, B](fa: List[A])(f: A => B): List[B] =
    fa match {
      case Nil => Nil
      case h :: t => f(h) :: map(t)(f)
    }
}

optionFunctor.map(Some(3))(_ + 1)
listFunctor.map(List(1, 2, 3))(_ * 2)

listFunctor.as(List(1, 3, 5), 10)
optionFunctor.as(Some(4), 11)


