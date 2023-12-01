package rop.jobsboard.http.routes

// These 2 imports should placed before the http4s imports
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import cats.data.OptionT
import org.http4s.headers.Authorization
import org.typelevel.ci.CIStringSyntax
import rop.jobsboard.core.Auth
import rop.jobsboard.domain.auth.{LoginInfo, NewPasswordInfo}
import rop.jobsboard.domain.security.{Authenticator, JwtToken}
import rop.jobsboard.domain.user.{NewUserInfo, User}
import rop.jobsboard.domain.{auth, user}
import rop.jobsboard.fixature.UserFixture
import tsec.authentication.{IdentityStore, JWTAuthenticator}
import tsec.jws.mac.JWTMac
import tsec.mac.jca.HMACSHA256

import scala.language.postfixOps
import scala.concurrent.duration.*
//
import cats.effect.*
import cats.implicits.*
import org.http4s.*
import org.http4s.implicits.*
import org.http4s.dsl.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.freespec.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class AuthRoutesSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with Http4sDsl[IO] with UserFixture {

  // todo make sure that only 'Person' from UserFixture already exists

  val mockedAuth: Auth[IO] = new Auth[IO] {
    override def login(email: String, password: String): IO[Option[JwtToken]] = ???

    override def signup(newUserInfo: user.NewUserInfo): IO[Option[user.User]] = ???

    override def changePassword(email: String, newPasswordInfo: auth.NewPasswordInfo): IO[Either[String, Option[user.User]]] = ???
  }

  val mockedAuthenticator: Authenticator[IO] = {
    // key for hashing
    val key = HMACSHA256.unsafeGenerateKey

    // identity store for retrieving users (in-memory map to find a user by key)
    val idStore: IdentityStore[IO, String, User] = { (email: String) =>
      if (email == someEmail) OptionT.pure(Person)
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

  extension (r: Request[IO])
    def withBearerToken(jwtToken: JwtToken): Request[IO] =
      r.putHeaders {
        val jwtString = JWTMac.toEncodedString[IO, HMACSHA256](jwtToken.jwt)
        // Authorization: Bearer {jwtString}
        Authorization(Credentials.Token(AuthScheme.Bearer, jwtString))
      }

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val authRoutes: HttpRoutes[IO] = AuthRoutes[IO](mockedAuth).routes

  "AuthRoutes" - {
    "should return a 401 - unauthorized if login fails" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/login")
            .withEntity(LoginInfo(someEmail, "wrong-password"))
        )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }

    "should return a 200 - Ok + a JWT if login is successful" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/login")
            .withEntity(LoginInfo(someEmail, somePassword))
        )
      } yield {
        response.status shouldBe Status.Ok
        response.headers.get(ci"Authorization") shouldBe defined
      }
    }

    "should return a 400 - BadRequest it the user to create already exists" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/users")
            .withEntity(PersonInfo)
        )
      } yield {
        response.status shouldBe Status.BadRequest
      }
    }

    "should return a 201 - Created if the user creation succeeds" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/users")
            .withEntity(AnotherUserInfo)
        )
      } yield {
        response.status shouldBe Status.Created
      }
    }

    "should return a 200 - Ok if logging out with a valid JWT token" in {
      for {
        jwtToken <- mockedAuthenticator.create(someEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/logout")
            .withBearerToken(jwtToken)
        )
      } yield {
        response.status shouldBe Status.Ok
      }
    }

    "should return a 401 - Unauthorized if logging out without a valid JWT token" in {
      for {
        jwtToken <- mockedAuthenticator.create(someEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/logout")
        )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }

    "should return a 404 - NotFound if changing password for user that doesn't exist" in {
      for {
        jwtToken <- mockedAuthenticator.create(anotherUserEmail) // anotherUserEmail does not exist
        response <- authRoutes.orNotFound
          .run(
            Request(method = Method.PUT, uri = uri"/auth/users/password")
              .withBearerToken(jwtToken)
              .withEntity(NewPasswordInfo(anotherUserPassword, "new-password"))
          )
      } yield {
        response.status shouldBe Status.NotFound
      }
    }

    "should return a 403 - Forbidden if old password is incorrect" in {
      for {
        jwtToken <- mockedAuthenticator.create(someEmail)
        response <- authRoutes.orNotFound
          .run(
            Request(method = Method.PUT, uri = uri"/auth/users/password")
              .withBearerToken(jwtToken)
              .withEntity(NewPasswordInfo("wrong-password", "new-password"))
          )
      } yield {
        response.status shouldBe Status.Forbidden
      }
    }

    "should return a 401 - Unauthorized if changing password without a JWT token" in {
      for {
        jwtToken <- mockedAuthenticator.create(someEmail)
        response <- authRoutes.orNotFound
          .run(
            Request(method = Method.PUT, uri = uri"/auth/users/password")
              .withEntity(NewPasswordInfo("wrong-password", "new-password"))
          )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }

    "should return a 200 - Ok if changing password for a user with a valid JWT and password" in {
      for {
        jwtToken <- mockedAuthenticator.create(someEmail) // someEmail exists
        response <- authRoutes.orNotFound
          .run(
            Request(method = Method.PUT, uri = uri"/auth/users/password")
              .withBearerToken(jwtToken)
              .withEntity(NewPasswordInfo(somePassword, "new-password"))
          )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }
  }
}
