package rop.jobsboard.core

import cats.data.OptionT
import cats.effect.{IO, Ref}
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.Inside
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import rop.jobsboard.config.SecurityConfig
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

  val securityConfig: SecurityConfig = SecurityConfig("secret", 1 day)

  val admin      = "admin@gmail.com"
  val validToken = "token_123"

  val mockedTokens: Tokens[IO] = new Tokens[IO] {
    override def getToken(email: String): IO[Option[String]] =
      if (email == Person.email) IO.pure(Some(validToken))
      else IO.pure(None)

    override def checkToken(email: String, token: String): IO[Boolean] =
      IO.pure(token == validToken)
  }

  val mockedEmails: Emails[IO] = new Emails[IO] {
    override def sendEmail(from: String, to: String, subject: String, content: String): IO[Unit] = IO.unit

    override def sendPasswordRecoveryEmail(from: String, to: String, token: String): IO[Unit] = IO.unit
  }

  // Since both methods in 'Emails' return IO[Unit], which makes it a bit difficult to test them, we can use an atomic
  // reference Ref to track which users send emails
  def probedEmails(users: Ref[IO, Set[String]]): Emails[IO] = new Emails[IO] {
    override def sendEmail(from: String, to: String, subject: String, content: String): IO[Unit] =
      users.modify(set => (set + to, ()))

    override def sendPasswordRecoveryEmail(from: String, to: String, token: String): IO[Unit] =
      sendEmail(from, to, "your subject", "your content")
  }

  "Auth 'algebra'" - {
    "login should return None if the user doesn't exist" in {
      val program = for {
        auth          <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        potentialUser <- auth.login("unknown_email@gmail.com", "password")
      } yield potentialUser

      program.asserting(_ shouldBe None)
    }

    "login should return None if the user exists but the password is wrong" in {
      val program = for {
        auth          <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        potentialUser <- auth.login(someEmail, "wrong_password")
      } yield potentialUser

      program.asserting(_ shouldBe None)
    }

    "login should return a user if the user exists and the password is correct" in {
      val program = for {
        auth          <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        potentialUser <- auth.login(someEmail, "somepassword")
      } yield potentialUser

      program.asserting(_ shouldBe Some(Person))
    }

    "signing up should not create a user with an existing email" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        potentialUser <- auth.signup(
          NewUserInfo(someEmail, "someotherpassword", Some("firstname"), Some("lastname"), Some("Corp"))
        )
      } yield potentialUser

      program.asserting(_ shouldBe None)
    }

    "signing up should create a new user" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
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
        auth          <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        potentialUser <- auth.changePassword("unknown@gmail.com", NewPasswordInfo("old-password", "new-password"))
      } yield potentialUser

      program.asserting(_ shouldBe Right(None))
    }

    "change_password should return an error if the user exists but the password is incorrect" in {
      val program = for {
        auth          <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        potentialUser <- auth.changePassword(someEmail, NewPasswordInfo("old-password", "new-password"))
      } yield potentialUser

      program.asserting(_ shouldBe Left("Invalid password"))
    }

    "change_password should correctly change password if all details are correct" in {
      val program = for {
        auth          <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        potentialUser <- auth.changePassword(someEmail, NewPasswordInfo("somepassword", "new-password"))
        isNicePassword <- potentialUser match
          case Right(Some(user)) => BCrypt.checkpwBool[IO]("somepassword", PasswordHash[BCrypt](Person.hashedPassword))
          case _                 => IO.pure(false)
      } yield isNicePassword

      program.asserting(_ shouldBe true)
    }

    "recover_password should fail for a user that does not exist, even if the the token is valid" in {
      val program = for {
        auth           <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        falseResponse1 <- auth.recoverPasswordFromToken(admin, "unknown_email@gmail.com", validToken, "this_is_a_new_password")
        falseResponse2 <- auth.recoverPasswordFromToken(
          admin,
          "unknown_email@gmail.com",
          "unknownToken",
          "this_is_a_new_password"
        )
      } yield (falseResponse1, falseResponse2)

      program.asserting(_ shouldBe (false, false))
    }

    "recover_password should fail for a user that exists, but the token is invalid" in {
      val program = for {
        auth          <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        falseResponse <- auth.recoverPasswordFromToken(admin, Person.email, "invalid_token", "this_is_a_new_password")
      } yield falseResponse

      program.asserting(_ shouldBe false)
    }

    "recover_password should succeed for a correct combination user/token" in {
      val program = for {
        auth         <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        trueResponse <- auth.recoverPasswordFromToken(admin, Person.email, validToken, "this_is_a_new_password")
      } yield trueResponse

      program.asserting(_ shouldBe true)
    }

    "sending_recovery_password should fail for a user that does not exist" in {
      val program = for {
        emailsSet            <- Ref.of[IO, Set[String]](Set())
        emails               <- IO(probedEmails(emailsSet))
        auth                 <- LiveAuth[IO](mockedUsers, mockedTokens, emails)
        result               <- auth.sendPasswordRecoveryToken(admin, "unknown_email@gmail.com")
        usersBeingSentEmails <- emailsSet.get
      } yield usersBeingSentEmails

      program.asserting(_ shouldBe empty)
    }

    "sending_recovery_password should succeed for a user that exists" in {
      val program = for {
        emailsSet            <- Ref.of[IO, Set[String]](Set())
        emails               <- IO(probedEmails(emailsSet))
        auth                 <- LiveAuth[IO](mockedUsers, mockedTokens, emails)
        result               <- auth.sendPasswordRecoveryToken(admin, Person.email)
        usersBeingSentEmails <- emailsSet.get
      } yield usersBeingSentEmails

      program.asserting(_ should contain(Person.email))
    }
  }
}
