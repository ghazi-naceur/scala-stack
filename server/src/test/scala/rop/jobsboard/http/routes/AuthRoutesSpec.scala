package rop.jobsboard.http.routes

// These 2 imports should placed before the http4s imports
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import cats.data.OptionT
import org.http4s.headers.Authorization
import org.typelevel.ci.CIStringSyntax
import rop.jobsboard.core.Auth
import rop.jobsboard.domain.auth.*
import rop.jobsboard.domain.security.{Authenticator, JwtToken}
import rop.jobsboard.domain.user.*
import rop.jobsboard.domain.{auth, user}
import rop.jobsboard.fixature.{SecuredRouteFixture, UserFixture}
import tsec.authentication.{IdentityStore, JWTAuthenticator}
import tsec.jws.mac.JWTMac
import tsec.mac.jca.HMACSHA256

import scala.language.postfixOps
import scala.concurrent.duration.*
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

class AuthRoutesSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with Http4sDsl[IO]
    with UserFixture
    with SecuredRouteFixture {

  val admin      = "admin@gmail.com"
  val validToken = "this_is_a_valid_token"

  val mockedAuth: Auth[IO] = probedAuth(None)

  // We use this atomic reference Ref to test the 2 methods for password recovery (by tracking the users and their tokens),
  // since both are returning IO[Unit].
  def probedAuth(userMap: Option[Ref[IO, Map[String, String]]]): Auth[IO] = new Auth[IO] {
    override def login(email: String, password: String): IO[Option[User]] =
      if (email == someEmail && password == somePassword) IO(Some(Person))
      else IO.pure(None)

    override def signup(newUserInfo: NewUserInfo): IO[Option[user.User]] =
      if (newUserInfo.email == anotherUserEmail)
        IO.pure(Some(AnotherUser))
      else IO.pure(None)

    override def changePassword(email: String, newPasswordInfo: auth.NewPasswordInfo): IO[Either[String, Option[user.User]]] =
      if (email == someEmail)
        if (newPasswordInfo.oldPassword == somePassword)
          IO.pure(Right(Some(Person)))
        else
          IO.pure(Left("Invalid password"))
      else IO.pure(Right(None))

    override def delete(email: String): IO[Boolean] =
      IO.pure(true)

    override def sendPasswordRecoveryToken(from: String, to: String): IO[Unit] =
      userMap
        .traverse { userMapRef =>
          userMapRef.modify { userMap =>
            (userMap + (to -> validToken), ())
          }
        }
        .map(_ => ())

    override def recoverPasswordFromToken(from: String, to: String, token: String, newPassword: String): IO[Boolean] =
      userMap
        .traverse { userMapRef =>
          userMapRef.get
            .map { userMap =>
              userMap.get(to).filter(_ == token) // Option[String]
            }                                    // IO[Option[String]]
            .map(_.nonEmpty)                     // IO[Boolean]
        }                                        // IO[Option[Boolean]]
        .map(_.getOrElse(false))                 // IO[Boolean]
  }

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val authRoutes: HttpRoutes[IO] = AuthRoutes[IO](mockedAuth, mockedAuthenticator).routes

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
        response.status shouldBe Status.Ok
      }
    }

    "should return a 401 - Unauthorized if a non-admin tries to delete a user" in {
      for {
        jwtToken <- mockedAuthenticator.create(anotherUserEmail) // anotherUserEmail is a non-admin
        response <- authRoutes.orNotFound
          .run(
            Request(method = Method.DELETE, uri = uri"/auth/users/someone@gmail.com")
              .withBearerToken(jwtToken)
          )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }

    "should return a 200 - Ok if an admin tries to delete a user" in {
      for {
        jwtToken <- mockedAuthenticator.create(someEmail) // someEmail is an admin
        response <- authRoutes.orNotFound
          .run(
            Request(method = Method.DELETE, uri = uri"/auth/users/someone@gmail.com")
              .withBearerToken(jwtToken)
          )
      } yield {
        response.status shouldBe Status.Ok
      }
    }

    "should return a 200 - Ok when resetting a password and email should be triggered" in {
      for {
        userMapRef <- Ref.of[IO, Map[String, String]](Map())
        auth       <- IO(probedAuth(Some(userMapRef)))
        routes     <- IO(AuthRoutes(auth, mockedAuthenticator).routes)
        response <- routes.orNotFound
          .run(
            Request(method = Method.POST, uri = uri"/auth/reset")
              .withEntity(ForgotPasswordInfo(admin, Person.email))
          )
        userMap <- userMapRef.get
      } yield {
        response.status shouldBe Status.Ok
        userMap should contain key (Person.email)
      }
    }

    "should return a 200 - Ok when recovering a password for a correct user/token combination" in {
      for {
        userMapRef <- Ref.of[IO, Map[String, String]](Map(Person.email -> validToken))
        auth       <- IO(probedAuth(Some(userMapRef)))
        routes     <- IO(AuthRoutes(auth, mockedAuthenticator).routes)
        response <- routes.orNotFound
          .run(
            Request(method = Method.POST, uri = uri"/auth/recover")
              .withEntity(RecoverPasswordInfo(admin, Person.email, validToken, "new_password"))
          )
      } yield {
        response.status shouldBe Status.Ok
      }
    }

    "should return a 403 - Forbidden when recovering a password for a user with an invalid token" in {
      for {
        userMapRef <- Ref.of[IO, Map[String, String]](Map(Person.email -> validToken))
        auth       <- IO(probedAuth(Some(userMapRef)))
        routes     <- IO(AuthRoutes(auth, mockedAuthenticator).routes)
        response <- routes.orNotFound
          .run(
            Request(method = Method.POST, uri = uri"/auth/recover")
              .withEntity(RecoverPasswordInfo(admin, Person.email, "invalid_token", "new_password"))
          )
      } yield {
        response.status shouldBe Status.Forbidden
      }
    }
  }
}
