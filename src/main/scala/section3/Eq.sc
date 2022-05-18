import cats._
import cats.implicits._

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  // default instance
  implicit val universalEq: Eq[Account] = Eq.fromUniversalEquals // ==

  object Instances {
    implicit def byIdEq(implicit eqLong: Eq[Long]): Eq[Account] = Eq.instance[Account]((a1, a2) => eqLong.eqv(a1.id, a2.id))
    implicit def byIdEq2(implicit eqLong: Eq[Long]): Eq[Account] = Eq.by(_.id)
    // compare account by number
    implicit def byNumberEq(implicit eqString: Eq[String]): Eq[Account] = Eq.by(_.number)
  }
}


val account1 = Account(1, "123-56", 1000, "Phil")
val account2 = Account(2, "123-56", 1500, "M1rtin")
Eq[Account].eqv(account1, account2)
Account.Instances.byIdEq.eqv(account1, account2)
Account.Instances.byNumberEq.eqv(account1, account2)

//import Account.Instances.byNumberEq
implicit val eqToUse = Account.Instances.byNumberEq
account1 === account2