package rop.jobsboard.http.routes

import cats.effect.*
import cats.implicits.*
import org.http4s.HttpRoutes
import org.http4s.server.Router
import org.typelevel.log4cats.Logger
import rop.jobsboard.core.*
import rop.jobsboard.http.validation.syntax.*

class AuthRoutes[F[_]: Concurrent: Logger] private (auth: Auth[F]) extends HttpValidationDsl[F] {

  // POST /auth/login { loginInfo } => 200 OK with JWT as Authorization: Bearer {jwt}
  private val loginRoute: HttpRoutes[F] = HttpRoutes.of[F] { case POST -> Root / "login" =>
    Ok("login")
  }

  // POST /auth/users { NewUserInfo } => 201 Created or BadRequest
  private val createUserRoute: HttpRoutes[F] = HttpRoutes.of[F] { case POST -> Root / "users" =>
    Ok("createUserRoute")
  }

  // PUT /auth/users/password { NewPasswordInfo } { Authorization: Bearer {jwt} } => 200 OK
  private val changePasswordRoute: HttpRoutes[F] = HttpRoutes.of[F] { case PUT -> Root / "users" / "password" =>
    Ok("changePasswordRoute")
  }

  // POST /auth/logout { Authorization: Bearer {jwt} } => 200 OK
  private val logoutRoute: HttpRoutes[F] = HttpRoutes.of[F] { case POST -> Root / "logout" =>
    Ok("logoutRoute")
  }

  val routes: HttpRoutes[F] = Router(
    "/auth" -> (loginRoute <+> createUserRoute <+> changePasswordRoute <+> logoutRoute)
  )
}

object AuthRoutes {
  def apply[F[_]: Concurrent: Logger](auth: Auth[F]) = new AuthRoutes[F](auth)
}
