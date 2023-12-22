package rop.jobsboard.core

import cats.effect.*
import cats.effect.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import rop.jobsboard.config.TokenConfig
import rop.jobsboard.fixature.UserFixture
import rop.jobsboard.domain.user.*
import rop.jobsboard.domain.auth.*

import scala.language.postfixOps
import scala.concurrent.duration.*

class TokensSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with DoobieSpec with UserFixture {

  override val initScript: String = "sql/recoverytokens.sql"

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  "Tokens 'algebra'" - {
    "should not create a new token for a non-existing user" in {
      transactor.use { xa =>
        val program = for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(10000000L))
          token  <- tokens.getToken("unknown@gmail.com")
        } yield token

        program.asserting(_ shouldBe None)
      }
    }

    "should create a new token for an existing user" in {
      transactor.use { xa =>
        val program = for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(10000000L))
          token  <- tokens.getToken(Person.email)
        } yield token

        program.asserting(_ shouldBe defined)
      }
    }

    "should not validate expired tokens" in {
      transactor.use { xa =>
        val program = for {
          tokens         <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(100L))
          potentialToken <- tokens.getToken(Person.email)
          _              <- IO.sleep(500 millis)
          isTokenValid <- potentialToken match {
            case Some(token) => tokens.checkToken(Person.email, token)
            case None        => IO.pure(false)
          }
        } yield isTokenValid

        program.asserting(_ shouldBe false)
      }
    }

    "should validate tokens that have not expired yet" in {
      transactor.use { xa =>
        val program = for {
          tokens         <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(1000000L))
          potentialToken <- tokens.getToken(Person.email)
          isTokenValid <- potentialToken match {
            case Some(token) => tokens.checkToken(Person.email, token)
            case None        => IO.pure(false)
          }
        } yield isTokenValid

        program.asserting(_ shouldBe true)
      }
    }

    "should only validate tokens for the user that generated them" in {
      transactor.use { xa =>
        val program = for {
          tokens         <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(1000000L))
          potentialToken <- tokens.getToken(Person.email)
          isKnownUserTokenValid <- potentialToken match {
            case Some(token) => tokens.checkToken(Person.email, token)
            case None        => IO.pure(true)
          }
          isUnknownUserTokenValid <- potentialToken match {
            case Some(token) => tokens.checkToken(AnotherUser.email, token)
            case None        => IO.pure(false)
          }
        } yield (isKnownUserTokenValid, isUnknownUserTokenValid)

        program.asserting { case (isKnownUserTokenValid, isUnknownUserTokenValid) =>
          isKnownUserTokenValid shouldBe true
          isUnknownUserTokenValid shouldBe false
        }
      }
    }
  }
}
