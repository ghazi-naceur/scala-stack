package rop.jobsboard.http.routes

import cats.data.Kleisli
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import cats.effect.*
import cats.implicits.*
import org.http4s.*
import org.http4s.server.Router
import org.typelevel.log4cats.Logger
import rop.jobsboard.core.*
import rop.jobsboard.domain.auth.{LoginInfo, NewPasswordInfo}
import rop.jobsboard.domain.security.*
import rop.jobsboard.domain.user.{NewUserInfo, User}
import rop.jobsboard.http.responses.FailureResponse
import rop.jobsboard.http.validation.syntax.*
import tsec.authentication.{SecuredRequestHandler, TSecAuthService, asAuthed}
import scala.language.implicitConversions
class AuthRoutes[F[_]: Concurrent: Logger] private (auth: Auth[F]) extends HttpValidationDsl[F] {

  private val authenticator                                                    = auth.authenticator
  private val securedHandler: SecuredRequestHandler[F, String, User, JwtToken] = SecuredRequestHandler(authenticator)

  // POST /auth/login { loginInfo } => 200 OK with JWT as Authorization: Bearer {jwt}
  private val loginRoute: HttpRoutes[F] = HttpRoutes.of[F] { case req @ POST -> Root / "login" =>
    req.validate[LoginInfo] { loginInfo =>
      val potentialJwtToken = for {
        potentialToken <- auth.login(loginInfo.email, loginInfo.password)
        _              <- Logger[F].info(s"User logging in: ${loginInfo.email}")
      } yield potentialToken

      potentialJwtToken.map {
        case Some(jwtToken) => authenticator.embed(Response(Status.Ok), jwtToken) // Authorization: Bearer token
        case None           => Response(Status.Unauthorized)
      }
    }
  }

  // POST /auth/users { NewUserInfo } => 201 Created or BadRequest
  private val createUserRoute: HttpRoutes[F] = HttpRoutes.of[F] { case req @ POST -> Root / "users" =>
    req.validate[NewUserInfo] { newUserInfo =>
      for {
        potentialNewUser <- auth.signup(newUserInfo)
        response <- potentialNewUser match {
          case Some(user) => Created(user.email)
          case None       => BadRequest(s"User with '${newUserInfo.email}' already exists")
        }
      } yield response
    }
  }

  // PUT /auth/users/password { NewPasswordInfo } { Authorization: Bearer {jwt} } => 200 OK
  private val changePasswordRoute: AuthRoute[F] = { case req @ PUT -> Root / "users" / "password" asAuthed user =>
    // 'asAuthed' is an object that has an 'unapply' method that takes a 'SecuredRequest'
    // as a wrapped of a regular http request and it will give us back a 'Request' (from Http4s) with an identifier, and in
    // our case the identifier will be the email by which that particular user tries to authenticate through the JWT
    req.request.validate[NewPasswordInfo] { newPasswordInfo =>
      for {
        potentialUser <- auth.changePassword(user.email, newPasswordInfo)
        response <- potentialUser match
          case Right(Some(_)) => Ok()
          case Right(None)    => NotFound(FailureResponse(s"User ${user.email} was not found"))
          case Left(error)    => Forbidden()
      } yield response
    }
  }

  // POST /auth/logout { Authorization: Bearer {jwt} } => 200 OK
  // this route will be wrapped in 'TSecAuthService' which will return 'Unauthorized' when having an invalid token
  private val logoutRoute: AuthRoute[F] = { case req @ POST -> Root / "logout" asAuthed _ =>
    val token = req.authenticator
    for {
      _        <- authenticator.discard(token)
      response <- Ok()
    } yield response
  }

  // DELETE /auth/users/"email"
  private val deleteUserRoute: AuthRoute[F] = { case req @ DELETE -> Root / "users" / email asAuthed _ =>
    auth.delete(email).flatMap {
      case true  => Ok()
      case false => NotFound()
    }
  }

  val unauthedRoutes: HttpRoutes[F] = loginRoute <+> createUserRoute
  val authedRoutes: HttpRoutes[F] = securedHandler.liftService(
    changePasswordRoute.restrictedTo(allRoles) |+|
      logoutRoute.restrictedTo(allRoles) |+|
      deleteUserRoute.restrictedTo(adminOnly)
  )

  val routes: HttpRoutes[F] = Router(
    "/auth" -> (unauthedRoutes <+> authedRoutes)
  )
}

object AuthRoutes {
  def apply[F[_]: Concurrent: Logger](auth: Auth[F]) = new AuthRoutes[F](auth)
}
