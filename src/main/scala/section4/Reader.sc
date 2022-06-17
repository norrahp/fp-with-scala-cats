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

case class Person(id: Long, name: String, email: String)
case class Account(id: Long, ownerId: Long)

trait AccountRepository {
  val accountRepository: Service

  trait Service {
    def findAccountById(id: Long): Account
    def saveAccount(account: Account): Unit
  }
}
trait LiveAccountRepository extends AccountRepository {
  override val accountRepository: Service = new Service {
    def findAccountById(id: Long): Account = Account(id, 2)
    def saveAccount(account: Account): Unit = {
      println(s"Account $account saved.")
    }
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
    def findPersonById(id: Long): Person = Person(2, "Phil", "phil@me.com")
  }
}

def findNextAccount(id: Long): Reader[AccountRepository, Account] =
  for {
    accountRepository <- Reader(identity[AccountRepository])
    account = accountRepository.accountRepository.findAccountById(id + 1)
  } yield account

val ar = new LiveAccountRepository with AccountRepository
findNextAccount(3).run(ar)

def findOwnerNameByAccountId(id: Long) =
  for {
    accountModule <- Reader(identity[AccountRepository])
    personModule <- Reader(identity[PersonRepository])
    account = accountModule.accountRepository.findAccountById(id)
    person = personModule.personRepository.findPersonById(account.ownerId)
  } yield person.name

//type Env = PersonRepository with AccountRepository
//val liveEnv: Env = new LivePersonRepository with LiveAccountRepository
//findOwnerNameByAccountId(1).run(liveEnv)

// 1. Define an EmailService module with a method sendEmail(address: String, text: String): Unit
// 2. Define a LiveEmailService trait with a dummy implementation for the module
// 3. Update Env and liveEnv to include this new module
// 4. Add an emailAddress field to the Person class and change the PersonRepository implementation accordingly
// 5. Add a saveAccount(account: Account): Unit method to the AccountRepository module
// 6. Add a dummy implementation for saveAccount in LiveAccountRepository
// 7. Implement an openAccount(accountId, ownerId) method that will create and save a new account,
// 8. and will notify the user via email. Use the reader monad, and the dependencies that you see fit.
// 9. Run the function using the live environment

trait EmailService {
  val emailService: Service

  trait Service {
    def sendEmail(address: String, text: String): Unit
  }
}
trait LiveEmailService extends EmailService {
  override val emailService: Service = new Service {
    def sendEmail(address: String, text: String): Unit =
      println(s"sending email $text to $address")
  }
}

def openAccount(accountId: Int, ownerId: Int) =
  for {
    accountModule <- Reader(identity[AccountRepository])
    personModule <- Reader(identity[PersonRepository])
    emailModule <- Reader(identity[EmailService])
    account = Account(accountId, ownerId)
    _ = accountModule.accountRepository.saveAccount(account)
    person = personModule.personRepository.findPersonById(account.ownerId)
    _ = emailModule.emailService.sendEmail(person.email, s"Account $accountId hase been created for you ${person.name}.")
  } yield account


type Env = PersonRepository with AccountRepository with EmailService
val liveEnv: Env = new LivePersonRepository with LiveAccountRepository with LiveEmailService
findOwnerNameByAccountId(1).run(liveEnv)

openAccount(1, 2).run(liveEnv)
