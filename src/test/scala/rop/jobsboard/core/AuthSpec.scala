package rop.jobsboard.core

import cats.data.OptionT
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.Inside
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import rop.jobsboard.domain.security.Authenticator
import rop.jobsboard.domain.user.*
import rop.jobsboard.domain.auth.*
import rop.jobsboard.fixature.UserFixture
import tsec.mac.jca.HMACSHA256
import tsec.authentication.{IdentityStore, JWTAuthenticator}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt

import scala.concurrent.duration.*
import scala.language.postfixOps

class AuthSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with UserFixture {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  private val mockedUsers: Users[IO] = new Users[IO] {
    override def find(email: String): IO[Option[User]] =
      if (email == userEmail) IO.pure(Some(Person))
      else IO.pure(None)

    override def create(user: User): IO[String] = IO.pure(user.email)

    override def update(user: User): IO[Option[User]] = IO.pure(Some(user))

    override def delete(email: String): IO[Boolean] = IO.pure(true)
  }

  val mockedAuthenticator: Authenticator[IO] = {
    // key for hashing
    val key = HMACSHA256.unsafeGenerateKey

    // identity store for retrieving users (in-memory map to find a user by key)
    val idStore: IdentityStore[IO, String, User] = { (email: String) =>
      if (email == userEmail) OptionT.pure(Person)
      else if (email == anotherUserEmail) OptionT.pure(AnotherUser)
      else OptionT.none[IO, User]
    }
    // jwt authenticator
    JWTAuthenticator.unbacked.inBearerToken(
      1 day,   // expiration of tokens
      None,    // max idle time (optional)
      idStore, // identity store
      key      // hash key
    )
  }

  "Auth 'algebra'" - {
    "login should return None if the user doesn't exist" in {
      val program = for {
        auth           <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        potentialToken <- auth.login("unknown_email@gmail.com", "password")
      } yield potentialToken

      program.asserting(_ shouldBe None)
    }

    "login should return None if the user exists but the password is wrong" in {
      val program = for {
        auth           <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        potentialToken <- auth.login(userEmail, "wrong_password")
      } yield potentialToken

      program.asserting(_ shouldBe None)
    }

    "login should return a token if the user exists and the password is correct" in {
      val program = for {
        auth           <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        potentialToken <- auth.login(userEmail, "somepassword")
      } yield potentialToken

      program.asserting(_ shouldBe defined)
    }

    "signing up should not create a user with an existing email" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        potentialUser <- auth.signup(
          NewUserInfo(userEmail, "someotherpassword", Some("firstname"), Some("lastname"), Some("Corp"))
        )
      } yield potentialUser

      program.asserting(_ shouldBe None)
    }

    "signing up should create a new user" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        potentialUser <- auth.signup(
          NewUserInfo("newemail@gmail.com", "newpassword", Some("new firstname"), Some("new lastname"), Some("New Corp"))
        )
      } yield potentialUser

      program.asserting {
        case Some(user) =>
          user.email shouldBe "newemail@gmail.com"
          user.firstName shouldBe Some("new firstname")
          user.lastName shouldBe Some("new lastname")
          user.company shouldBe Some("New Corp")
          user.role shouldBe Role.RECRUITER
        case _ => fail()
      }
    }

    "change_password should return None if the user doesn't exist" in {
      val program = for {
        auth          <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        potentialUser <- auth.changePassword("unknown@gmail.com", NewPasswordInfo("old-password", "new-password"))
      } yield potentialUser

      program.asserting(_ shouldBe Right(None))
    }

    "change_password should return an error if the user exists but the password is incorrect" in {
      val program = for {
        auth          <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        potentialUser <- auth.changePassword(userEmail, NewPasswordInfo("old-password", "new-password"))
      } yield potentialUser

      program.asserting(_ shouldBe Left("Invalid password"))
    }

    "change_password should correctly change password if all details are correct" in {
      val program = for {
        auth          <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        potentialUser <- auth.changePassword(userEmail, NewPasswordInfo("somepassword", "new-password"))
        isNicePassword <- potentialUser match
          case Right(Some(user)) => BCrypt.checkpwBool[IO]("somepassword", PasswordHash[BCrypt](Person.hashedPassword))
          case _                 => IO.pure(false)
      } yield isNicePassword

      program.asserting(_ shouldBe true)
    }
  }
}
