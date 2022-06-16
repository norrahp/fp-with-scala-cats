import cats._
import cats.implicits._
import cats.data._

val signReader: Reader[Int, String] = Reader(n => if(n > 0) "positive" else if(n < 0) "negative" else "zero")// Int => String

signReader.run(-1)

val parityReader: Reader[Int, String] = Reader(n => if (n % 2 == 0) "even" else "odd")
parityReader.run(23)

val descriptionReader: Reader[Int, String] =
  for {
    sign <- signReader
    parity <- parityReader
  } yield s"$sign and $parity"

descriptionReader.run(-2)

val addOneReader: Reader[Int, Int] =
  for {
    env <- Reader((x: Int) => x) // ask..or
//    env <- Reader(identity[Int]) // ask
  } yield env + 1

case class Person(id: Long, name: String)
case class Account(id: Long, ownerId: Long)

trait AccountRepository {
  val accountRepository: Service

  trait Service {
    def findAccountById(id: Long): Account
  }
}
trait LiveAccountRepository extends AccountRepository {
  override val accountRepository: Service = new Service {
    def findAccountById(id: Long): Account = Account(id, 2)
  }
}
trait PersonRepository {
  val personRepository: Service

  trait Service {
    def findPersonById(id: Long): Person
  }
}
trait LivePersonRepository extends PersonRepository {
  override val personRepository: Service = new Service {
    def findPersonById(id: Long): Person = Person(2, "Phil")
  }
}

def findNextAccount(id: Long): Reader[AccountRepository, Account] =
  for {
    accountRepository <- Reader(identity[AccountRepository])
    account = accountRepository.accountRepository.findAccountById(id + 1)
  } yield account

def findOwnerNameByAccountId(id: Long) =
  for {
    accountModule <- Reader(identity[AccountRepository])
    personModule <- Reader(identity[PersonRepository])
    account = accountModule.accountRepository.findAccountById(id)
    person = personModule.personRepository.findPersonById(account.ownerId)
  } yield person.name

type Env = PersonRepository with AccountRepository
val liveEnv: Env = new LivePersonRepository with LiveAccountRepository

findOwnerNameByAccountId(1).run(liveEnv)